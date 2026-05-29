//! A BSD-styled CLI parser.  
//!
//! example usage:
//! ```
//! # use std::process::ExitCode;
//! # fn main() -> ExitCode {
//! #[derive(supershorty::Args)]
//! #[args(name = "myapp", allow_no_args = true)]
//! struct MyArgs {
//!     #[arg(flag = 'f', help = "some flag")]
//!     flag: bool,
//!     #[arg(flag = 'o', help = "some option")]
//!     option: Option<String>,
//! }
//!
//! let args = match MyArgs::parse() {
//!     Ok(args) => args,
//!     Err(e) => return e,
//! };
//!
//! # let (is_err, is_help) = (false, false);
//!
//! if is_err {
//!     MyArgs::usage();
//!     return ExitCode::FAILURE;
//! } else if is_help {
//!     MyArgs::help();
//!     return ExitCode::SUCCESS;
//! }
//! # ExitCode::SUCCESS
//! # }
//! ```
extern crate proc_macro;

use darling::FromDeriveInput;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DeriveInput, Error, Expr, Fields, Lit, Meta, PathArguments, Token, Type, TypePath,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(args))]
struct ArgsAttr {
    name: String,
    allow_no_args: Option<bool>,
}

#[derive(Default)]
struct ArgAttr {
    flag: Option<char>,
    help: Option<String>,
}

impl Parse for ArgAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut out = Self::default();

        let metas: Punctuated<Meta, Token![,]> = Punctuated::parse_terminated(input)?;

        for meta in metas {
            match meta {
                Meta::NameValue(nv) if nv.path.is_ident("flag") => match nv.value {
                    Expr::Lit(expr) => match expr.lit {
                        Lit::Char(ch) => out.flag = Some(ch.value()),
                        _ => {
                            return Err(Error::new_spanned(expr, "expected char literal"));
                        }
                    },
                    _ => {
                        return Err(Error::new_spanned(nv, "expected char literal"));
                    }
                },

                Meta::NameValue(nv) if nv.path.is_ident("help") => match nv.value {
                    Expr::Lit(expr) => match expr.lit {
                        Lit::Str(s) => out.help = Some(s.value()),
                        _ => {
                            return Err(Error::new_spanned(expr, "expected string literal"));
                        }
                    },
                    _ => {
                        return Err(Error::new_spanned(nv, "expected string literal"));
                    }
                },

                _ => {
                    return Err(Error::new_spanned(meta, "unsupported attribute"));
                }
            }
        }

        Ok(out)
    }
}

struct FieldInfo<'a> {
    ident: &'a syn::Ident,
    flag: char,
    help: String,
    is_option: bool,
    value_name: String,
    inner_ty: Option<&'a Type>,
}

fn extract_option_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(TypePath { path, .. }) = ty else {
        return None;
    };

    let seg = path.segments.last()?;

    if seg.ident != "Option" {
        return None;
    }

    let PathArguments::AngleBracketed(args) = &seg.arguments else {
        return None;
    };

    if args.args.len() != 1 {
        return None;
    }

    match &args.args[0] {
        syn::GenericArgument::Type(ty) => Some(ty),
        _ => None,
    }
}

fn parse_field(field: &syn::Field) -> syn::Result<FieldInfo<'_>> {
    let ident = field
        .ident
        .as_ref()
        .ok_or_else(|| Error::new_spanned(field, "expected named field"))?;

    let mut attr: Option<ArgAttr> = None;

    for item in &field.attrs {
        if item.path().is_ident("arg") {
            let Meta::List(list) = &item.meta else {
                return Err(Error::new_spanned(item, "expected #[arg(...)]"));
            };

            attr = Some(syn::parse2(list.tokens.clone())?);
        }
    }

    let attr = attr.ok_or_else(|| Error::new_spanned(field, "missing #[arg(...)] attribute"))?;

    let flag = attr
        .flag
        .ok_or_else(|| Error::new_spanned(field, "missing `flag = 'x'`"))?;
    if !flag.is_ascii() {
        return Err(Error::new_spanned(field, "flag must be ASCII"));
    }

    let inner_ty = extract_option_inner(&field.ty);

    Ok(FieldInfo {
        ident,
        flag,
        help: attr.help.unwrap_or_else(|| "UNDOCUMENTED OPTION".into()),
        is_option: inner_ty.is_some(),
        value_name: ident.to_string(),
        inner_ty,
    })
}

enum ValueKind {
    Parse,
    String,
    BoxStr,
}

fn classify_value_type(ty: &Type) -> ValueKind {
    match ty {
        Type::Path(tp) => {
            let Some(seg) = tp.path.segments.last() else {
                return ValueKind::Parse;
            };

            if seg.ident == "String" {
                return ValueKind::String;
            }

            if seg.ident == "Box" {
                if let PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(Type::Path(inner))) = args.args.first() {
                        if inner.path.is_ident("str") {
                            return ValueKind::BoxStr;
                        }
                    }
                }
            }

            ValueKind::Parse
        }

        _ => ValueKind::Parse,
    }
}

fn format_usage(prefix: &str, parts: &[String], width: usize) -> String {
    let indent = " ".repeat(prefix.len());

    let mut out = String::from(prefix);
    let mut line_len = prefix.len();

    for (i, part) in parts.iter().enumerate() {
        let sep = if i == 0 { "" } else { " " };

        if line_len + sep.len() + part.len() > width {
            out.push('\n');
            out.push_str(&indent);
            out.push_str(part);

            line_len = indent.len() + part.len();
        } else {
            out.push_str(sep);
            out.push_str(part);

            line_len += sep.len() + part.len();
        }
    }

    out
}

#[proc_macro_derive(Args, attributes(args, arg))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match derive_impl(input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn derive_impl(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let args = ArgsAttr::from_derive_input(&input)?;

    if args.name.is_empty() {
        return Err(Error::new_spanned(
            input.ident,
            "missing #[args(name = \"...\")]",
        ));
    }

    let Data::Struct(data) = &input.data else {
        return Err(Error::new_spanned(
            input.ident,
            "Args can only be derived for structs",
        ));
    };

    let Fields::Named(fields) = &data.fields else {
        return Err(Error::new_spanned(
            data.fields.clone(),
            "expected named fields",
        ));
    };

    let parsed_fields = {
        let r: syn::Result<Vec<_>> = fields.named.iter().map(parse_field).collect();
        r?
    };

    let struct_name = &input.ident;
    let cli_name = args.name;
    let allow_no_args = args.allow_no_args.unwrap_or(false);

    // =========================
    // init fields
    // =========================

    let init_fields = parsed_fields.iter().map(|field| {
        let ident = field.ident;

        if let Some(inner) = field.inner_ty {
            quote! {
                #ident: None::<#inner>
            }
        } else {
            quote! {
                #ident: false
            }
        }
    });

    // =========================
    // parse arms
    // =========================

    let parse_arms = parsed_fields.iter().map(|field| {
        let ident = field.ident;
        let flag = field.flag;

        if let Some(inner_ty) = field.inner_ty {
            let parse_expr = match classify_value_type(inner_ty) {
                ValueKind::String => {
                    quote! { value }
                }

                ValueKind::BoxStr => {
                    quote! { value.into_boxed_str() }
                }

                ValueKind::Parse => {
                    quote! {
                        value.parse::<#inner_ty>().map_err(|_| {
                            Self::usage();
                            std::process::ExitCode::FAILURE
                        })?
                    }
                }
            };

            quote! {
                #flag => {
                    let value = if pos + 1 < bytes.len() {
                        arg[pos + 1..].to_owned()
                    } else {
                        args.next().ok_or_else(|| {
                            Self::usage();
                            std::process::ExitCode::FAILURE
                        })?
                    };

                    instance.#ident = Some(#parse_expr);

                    break;
                }
            }
        } else {
            quote! {
                #flag => {
                    instance.#ident = true;

                    pos += 1;
                }
            }
        }
    });

    // =========================
    // help text
    // =========================

    let mut help_lines: Vec<_> = parsed_fields
        .iter()
        .map(|field| {
            let flag = field.flag;
            let help = &field.help;
            let left = format!("-{}", flag);

            (
                flag,
                format!("        {:<width$}{}", left, help, width = 16,),
            )
        })
        .collect();

    let left = "-h";

    help_lines.push((
        'h',
        format!(
            "        {:<width$}{}",
            left,
            "print this help message",
            width = 16,
        ),
    ));

    help_lines.sort_by_key(|(flag, _)| flag.to_ascii_lowercase());

    let mut help_text = String::new();

    for (idx, (_, line)) in help_lines.into_iter().enumerate() {
        if idx != 0 {
            help_text.push('\n');
        }

        help_text.push_str(&line);
    }

    // =========================
    // usage
    // =========================

    let mut bool_flags = vec!['h'];
    let mut option_parts = Vec::new();

    for field in &parsed_fields {
        if field.is_option {
            option_parts.push(format!("[-{} {}]", field.flag, field.value_name,));
        } else {
            bool_flags.push(field.flag);
        }
    }

    bool_flags.sort_unstable_by_key(|c| (c.to_ascii_lowercase(), *c));

    option_parts.sort_unstable();

    let mut usage_parts = Vec::new();

    if !bool_flags.is_empty() {
        let flags: String = bool_flags.into_iter().collect();

        usage_parts.push(format!("[-{}]", flags));
    }

    usage_parts.extend(option_parts);

    let usage = format_usage(&format!("usage: {} ", cli_name), &usage_parts, 65);

    Ok(quote! {
        impl #struct_name {
            pub fn usage() {
                eprintln!(#usage);
            }

            pub fn help() {
                Self::usage();
                eprintln!("Command Summary:");
                eprintln!(#help_text);
            }

            pub fn parse()
                -> Result<Self, std::process::ExitCode>
            {
                let mut args =
                    std::env::args().skip(1).peekable();

                if !#allow_no_args && args.peek().is_none() {
                    Self::usage();
                    return Err(std::process::ExitCode::FAILURE);
                }

                let mut instance = Self {
                    #(#init_fields),*
                };

                while let Some(arg) = args.next() {
                    if !arg.starts_with('-') || arg.len() < 2  {
                        Self::usage();
                        return Err(std::process::ExitCode::FAILURE);
                    }

                    let bytes = arg.as_bytes();
                    let mut pos = 1;

                    while pos < bytes.len() {
                        let ch = bytes[pos];

                        if !ch.is_ascii() {
                            Self::usage();
                            return Err(std::process::ExitCode::FAILURE);
                        }

                        match ch as char {
                            'h' => {
                                Self::help();
                                return Err(
                                    std::process::ExitCode::SUCCESS
                                );
                            }

                            #(#parse_arms)*

                            _ => {
                                Self::usage();
                                return Err(
                                    std::process::ExitCode::FAILURE
                                );
                            }
                        }
                    }
                }

                Ok(instance)
            }
        }
    })
}
