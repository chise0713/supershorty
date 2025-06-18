//! A BSD-styled CLI parser.  
//!
//! example usage:
//! ```
//! #[derive(Args)]
//! #[args(name = "myapp", allow_no_args = true)]
//! struct MyArgs {
//!     #[arg(flag = 'f', help = "some flag")]
//!     flag: bool,
//!     #[arg(flag = 'o', help = "some option")]
//!     option: Option<String>,
//! }
//! ```
//!
//! ```
//! let args = match MyArgs::parse() {
//!     Ok(args) => args,
//!     Err(e) => return e,
//! };
//! if is_err {
//!     MyArgs::usage();
//!     return std::process::ExitCode::FAILURE;
//! } else if is_help {
//!     MyArgs::help();
//!     return std::process::ExitCode::SUCCESS;
//! }
//! ```

extern crate proc_macro;
use darling::FromDeriveInput;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DeriveInput, Error, Expr, Fields, Lit, PathArguments, Token, Type, TypePath,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

#[derive(FromDeriveInput, Default)]
#[darling(default, attributes(args))]
struct ArgsAttr {
    name: String,
    allow_no_args: Option<bool>,
}

struct ArgAttr {
    flag: Option<char>,
    help: Option<String>,
}

impl Parse for ArgAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut flag = None;
        let mut help = None;

        let metas = syn::punctuated::Punctuated::<syn::Meta, Token![,]>::parse_terminated(input)?;

        for meta in metas {
            match meta {
                syn::Meta::NameValue(nv) => {
                    if nv.path.is_ident("flag") {
                        let nv_value = nv.value;
                        if let Expr::Lit(syn::ExprLit {
                            lit: Lit::Char(lit_char),
                            ..
                        }) = nv_value
                        {
                            flag = Some(lit_char.value());
                        } else {
                            return Err(Error::new_spanned(
                                nv_value,
                                "Expected char literal for 'flag' (e.g., flag = 'c')",
                            ));
                        }
                    } else if nv.path.is_ident("help") {
                        let nv_value = nv.value;
                        if let Expr::Lit(syn::ExprLit {
                            lit: Lit::Str(lit_str),
                            ..
                        }) = nv_value
                        {
                            help = Some(lit_str.value());
                        } else {
                            return Err(Error::new_spanned(
                                nv_value,
                                "Expected string literal for 'help' (e.g., help = \"description\")",
                            ));
                        }
                    } else {
                        return Err(Error::new_spanned(
                            nv,
                            "Unsupported attribute key. Expected `flag` or `help`",
                        ));
                    }
                }
                _ => {
                    return Err(Error::new_spanned(
                        meta,
                        "Unsupported attribute format. Expected `key = value` (e.g., flag = 'c')",
                    ));
                }
            }
        }

        Ok(ArgAttr { flag, help })
    }
}

#[proc_macro_derive(Args, attributes(arg, args))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let args = ArgsAttr::from_derive_input(&input)
        .expect("wrong input");
    let name = args.name.into_boxed_str();
    let allow_no_args = args.allow_no_args.unwrap_or(false);
    if name.is_empty() {
        panic!("the `name` in `args` attribute is required for `Args` derive macro");
    }
    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields_named) => &fields_named.named,
            _ => {
                panic!("incorrect struct format");
            }
        },
        _ => {
            panic!("`Args` can only be derived for structs");
        }
    };

    let init_fields: Box<[proc_macro2::TokenStream]> = fields
        .iter()
        .map(|field| {
            let field_ident = field
                .ident
                .as_ref()
                .expect("Named fields should always have an identifier");
            let field_type = &field.ty;
            if let Type::Path(TypePath { path, .. }) = field_type {
                if let Some(segment) = path.segments.last() {
                    if segment.ident == "Option" {
                        if let PathArguments::AngleBracketed(args) = &segment.arguments {
                            if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first()
                            {
                                return quote! { #field_ident: None::<#inner_type> };
                            }
                        }
                        let error = syn::Error::new_spanned(
                            field_type,
                            "Option type must have exactly one type argument (e.g., Option<T>)",
                        )
                        .to_compile_error();
                        return quote! { #field_ident: #error };
                    }
                }
            }
            quote! { #field_ident: false }
        })
        .collect();

    let match_arms: Box<[proc_macro2::TokenStream]> = fields
        .iter()
        .map(|field| {
            let field_ident = field.ident.as_ref().unwrap();
            let field_name = field_ident.to_string();
            let mut flag = None;
            for attr in &field.attrs {
                if attr.path().is_ident("arg") {
                    if let syn::Meta::List(meta_list) = &attr.meta {
                        let arg: ArgAttr = syn::parse2(meta_list.tokens.clone())
                            .expect("failed to parse field attributes");
                        flag = arg.flag;
                    }
                }
            }
            let flag = flag.unwrap_or(field_name.chars().next().unwrap());
            let field_type = &field.ty;
            let mut last_segment = None;
            if let Type::Path(TypePath { path, .. }) = field_type {
                last_segment = path.segments.last();
            };
            let is_option = last_segment.is_some_and(|seg| seg.ident == "Option");

            if is_option {
                let inner_type =
                    if let PathArguments::AngleBracketed(args) = &last_segment.unwrap().arguments {
                        if args.args.len() != 1 {
                            panic!("Option type must have exactly one type argument");
                        }
                        let mut inner_type = None;
                        if let syn::GenericArgument::Type(ty) = &args.args[0] {
                            inner_type = Some(ty);
                        }
                        inner_type.expect("Option type must have exactly one type argument")
                    } else {
                        panic!("Option type must be generic");
                    };
                let inner_type_str = quote!(#inner_type).to_string();
                let parse = if inner_type_str.contains(" str ") {
                    quote! {
                        .into_boxed_str().into()
                    }
                } else {
                    quote! {
                        .parse().map_err(|_| {
                            Self::usage();
                            std::process::ExitCode::FAILURE
                        })?
                    }
                };
                let parse = quote! {
                    let value: #inner_type = if flags.iter().nth(pos + 1).is_none() {
                        args.next().ok_or_else(|| {
                            Self::usage();
                            std::process::ExitCode::FAILURE
                        })?#parse
                    } else {
                        let opos = pos + 1;
                        pos += flags.len() - 1;
                        flags[opos..].iter().collect::<String>()#parse
                    };
                };
                quote! {
                    #flag => {
                        #parse
                        instance.#field_ident = Some(value);
                    }
                }
            } else {
                quote! {
                    #flag => {
                        instance.#field_ident = true;
                    }
                }
            }
        })
        .collect();

    let mut help_entries: Box<[(char, proc_macro2::TokenStream)]> = fields
        .iter()
        .map(|field| {
            let mut flag = None;
            let mut help_override = None;
            for attr in &field.attrs {
                if attr.path().is_ident("arg") {
                    if let syn::Meta::List(meta_list) = &attr.meta {
                        let arg: ArgAttr = syn::parse2(meta_list.tokens.clone())
                            .expect("failed to parse field attributes");
                        flag = arg.flag;
                        help_override = arg.help.map(|s| s.into_boxed_str());
                    }
                }
            }
            let is_option = matches!(&field.ty, Type::Path(type_path) if 
                type_path.path.segments.last().is_some_and( |seg| seg.ident == "Option"));
            let text = help_override.unwrap_or_else(|| {
                if is_option {
                    Box::from("VALUE PLACEHOLDER")
                } else {
                    Box::from("BOOLEAN PLACEHOLDER")
                }
            });
            let flag = flag.unwrap_or(
                field
                    .ident
                    .as_ref()
                    .unwrap()
                    .to_string()
                    .chars()
                    .next()
                    .unwrap(),
            );
            (
                flag,
                quote! {
                    eprintln!("        -{}              {}", #flag, #text);
                },
            )
        })
        .chain([(
            'h',
            quote! {
                eprintln!("        -h              prints this help message");
            },
        )])
        .collect();
    help_entries.sort_by_key(|(flag, _)| *flag);
    let field_helps: Box<[proc_macro2::TokenStream]> =
        help_entries.into_iter().map(|(_, ts)| ts).collect();

    let mut bool_flags: Box<[char]> = fields
        .iter()
        .filter(|field| {
            !matches!(&field.ty, Type::Path(type_path) if 
                type_path.path.segments.last().is_some_and(|seg| seg.ident == "Option"))
        })
        .map(|field| {
            let field_name = field.ident.as_ref().unwrap().to_string();
            let mut flag = None;
            for attr in &field.attrs {
                if attr.path().is_ident("arg") {
                    if let syn::Meta::List(meta_list) = &attr.meta {
                        let arg: ArgAttr = syn::parse2(meta_list.tokens.clone())
                            .expect("failed to parse field attributes");
                        flag = arg.flag;
                    }
                }
            }
            flag.unwrap_or(field_name.chars().next().unwrap())
        })
        .chain(['h'])
        .collect();
    bool_flags.sort();
    let available_bool = if bool_flags.is_empty() {
        "".to_string()
    } else {
        format!("[-{}]", bool_flags.into_iter().collect::<Box<str>>())
    };

    let mut value_options: Box<[(char, Box<str>)]> = fields
        .iter()
        .filter(|field| {
            matches!(&field.ty, Type::Path(type_path) if 
                type_path.path.segments.last().is_some_and( |seg| seg.ident == "Option"))
        })
        .map(|field| {
            let field_name = field.ident.as_ref().unwrap().to_string().into_boxed_str();
            let mut flag = None;
            for attr in &field.attrs {
                if attr.path().is_ident("arg") {
                    if let syn::Meta::List(meta_list) = &attr.meta {
                        let arg: ArgAttr = syn::parse2(meta_list.tokens.clone())
                            .expect("failed to parse field attributes");
                        flag = arg.flag;
                    }
                }
            }
            let flag = flag.unwrap_or(field_name.chars().next().unwrap());
            (flag, field_name)
        })
        .collect();
    value_options.sort_by_key(|(flag, _)| *flag);
    let available_values = value_options
        .into_iter()
        .map(|(flag, field_name)| format!("[-{} {}]", flag, field_name))
        .collect::<Box<[_]>>()
        .join(" ")
        .into_boxed_str();

    let help_definition = quote! {
        fn help() {
            Self::usage();
            eprintln!("Command Summary:");
            #(#field_helps)*
        }
    };

    let usage_definition = quote! {
        fn usage() {
            eprintln!("usage: {} {} {}", #name, #available_bool, #available_values);
        }
    };

    let expanded = quote! {
        pub trait Usage {
            fn usage();
        }

        impl Usage for #struct_name {
            #usage_definition
        }

        pub trait Help {
            fn help()
            where
                Self: Usage;
        }

        impl Help for #struct_name {
            #help_definition
        }

        pub trait Parse {
            fn parse() -> Result<Self, std::process::ExitCode> where Self: Sized;
        }

        impl Parse for #struct_name {
            fn parse() -> Result<Self, std::process::ExitCode> {
                let mut instance = #struct_name { #(#init_fields),* };
                let mut args = std::env::args().skip(1);
                if !#allow_no_args && args.len() == 0 {
                    Self::usage();
                    return Err(std::process::ExitCode::FAILURE);
                }
                while let Some(arg) = args.next() {
                    if !arg.starts_with('-') {
                        Self::usage();
                        return Err(std::process::ExitCode::FAILURE);
                    }

                    let flags = arg.chars().skip(1).collect::<Box<[char]>>();
                    if flags.is_empty() {
                        Self::usage();
                        return Err(std::process::ExitCode::FAILURE);
                    }
                    if flags.iter().any(|&ch| ch == 'h') {
                        Self::help();
                        return Err(std::process::ExitCode::SUCCESS);
                    }
                    let mut pos = 0;

                    while pos < flags.len() {
                        match flags[pos] {
                            #(#match_arms)*
                            _ => {
                                Self::usage();
                                return Err(std::process::ExitCode::FAILURE);
                            },
                        }
                        pos += 1;
                    }
                }

                Ok(instance)
            }
        }
    };

    TokenStream::from(expanded)
}
