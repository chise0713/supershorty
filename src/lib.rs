//! A BSD-styled CLI parser.  
//!
//! example usage:
//! ```
//! #[derive(Args)]
//! #[name("myapp")]
//! struct MyArgs {
//!     #[arg(flag = 'f', help = "some flag")]
//!     flag: bool,
//!     #[arg(flag = 'o', help = "some option")]
//!     option: Option<String>,
//! }
//! ```
//!
//! ```
//! let args = MyArgs::parse();
//! if is_err {
//!     MyArgs::usage();
//!     std::process::exit(1);
//! } else if is_help {
//!     MyArgs::help();
//!     std::process::exit(0);
//! }
//! ```

extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Lit, PathArguments, Type, TypePath, parse_macro_input};

#[proc_macro_derive(Args, attributes(arg, name))]
pub fn f(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let name = input
        .attrs
        .iter()
        .find_map(|attr| {
            if attr.path().is_ident("name") {
                Some(attr.parse_args::<syn::LitStr>().unwrap().value())
            } else {
                None
            }
        })
        .unwrap_or_else(|| "program".to_string());
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
            let field_ident = field.ident.as_ref().unwrap();
            let field_type = &field.ty;
            let mut last_segment = None;
            if let Type::Path(TypePath { path, .. }) = field_type {
                last_segment = path.segments.last();
            };
            let is_option = last_segment.is_some_and(|seg| seg.ident == "Option");
            let init_expr = if is_option {
                if let PathArguments::AngleBracketed(args) = &last_segment.unwrap().arguments {
                    if args.args.len() != 1 {
                        panic!("Option type must have exactly one type argument");
                    }
                    let mut inner_type = None;
                    if let syn::GenericArgument::Type(ty) = &args.args[0] {
                        inner_type = Some(ty);
                    }
                    let inner_type =
                        inner_type.expect("Option type must have exactly one type argument");
                    quote! { None::<#inner_type> }
                } else {
                    panic!("Option type must be generic");
                }
            } else {
                quote! { false }
            };
            quote! { #field_ident: #init_expr }
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
                    let _ = attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("flag") {
                            let lit: Lit = meta.value().unwrap().parse().unwrap();
                            if let Lit::Char(char_lit) = lit {
                                flag = Some(char_lit.value());
                            }
                        }
                        Ok(())
                    });
                }
            }
            let flag = flag.unwrap_or(field_name.chars().next().unwrap());
            let field_type = &field.ty;
            let mut last_segment = None;
            if let Type::Path(TypePath { path, .. }) = field_type {
                last_segment = path.segments.last();
            };
            let is_option = last_segment.map_or(false, |seg| seg.ident == "Option");

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
                let parse = if inner_type_str.contains("Box < str >") {
                    quote! {
                        .into_boxed_str()
                    }
                } else {
                    quote! {
                        .parse().unwrap_or_else(|_| {
                            Self::usage();
                            std::process::exit(1);
                        })
                    }
                };
                let parse = quote! {
                    let value: #inner_type = if flags.iter().nth(pos + 1).is_none() {
                        args.next().unwrap_or_else(|| {
                            Self::usage();
                            std::process::exit(1);
                        })#parse
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
            let mut flag = field.ident.as_ref().unwrap().to_string().chars().next().unwrap();
            let mut help_override = None;
            for attr in &field.attrs {
                if attr.path().is_ident("arg") {
                    let _ = attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("flag") {
                            let lit: Lit = meta.value().unwrap().parse().unwrap();
                            if let Lit::Char(char_lit) = lit {
                                if char_lit.value() == 'h' {
                                    panic!("'h' is reserved for help flag, please use a different character");
                                }
                                flag = char_lit.value();
                            }
                        } else if meta.path.is_ident("help") {
                            let lit: Lit = meta.value().unwrap().parse().unwrap();
                            if let Lit::Str(lit_str) = lit {
                                help_override = Some(lit_str.value().into_boxed_str());
                            }
                        }
                        Ok(())
                    });
                }
            }
            let is_option = matches!(&field.ty, Type::Path(type_path) if 
                type_path.path.segments.last().is_some_and( |seg| seg.ident == "Option"));
            let text = help_override.unwrap_or_else(|| {
                if is_option {
                    Box::from("requires a value")
                } else {
                    Box::from("boolean flag")
                }
            });
            (flag, quote! {
                eprintln!("        -{}              {}", #flag, #text);
            })
        }).chain([
            ('h', quote! {
                eprintln!("        -h              prints this help message");
            })]
        )
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
                    let _ = attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("flag") {
                            let lit: Lit = meta.value().unwrap().parse().unwrap();
                            if let Lit::Char(char_lit) = lit {
                                flag = Some(char_lit.value());
                            }
                        }
                        Ok(())
                    });
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
                    let _ = attr.parse_nested_meta(|meta| {
                        if meta.path.is_ident("flag") {
                            let lit: Lit = meta.value().unwrap().parse().unwrap();
                            if let Lit::Char(char_lit) = lit {
                                flag = Some(char_lit.value());
                            }
                        }
                        Ok(())
                    });
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
            fn parse() -> Self where Self: Sized;
        }

        impl Parse for #struct_name {
            fn parse() -> Self {
                let mut instance = #struct_name { #(#init_fields),* };
                let mut args = std::env::args().skip(1);
                if args.len() == 0 {
                    Self::usage();
                    std::process::exit(1);
                }
                while let Some(arg) = args.next() {
                    if !arg.starts_with('-') {
                        Self::usage();
                        std::process::exit(1);
                    }

                    let flags = arg.chars().skip(1).collect::<Box<[char]>>();
                    if flags.is_empty() {
                        Self::usage();
                        std::process::exit(1);
                    }
                    if flags.iter().any(|&ch| ch == 'h') {
                        Self::help();
                        std::process::exit(0);
                    }
                    let mut pos = 0;

                    while pos < flags.len() {
                        match flags[pos] {
                            #(#match_arms)*
                            _ => {
                                Self::usage();
                                std::process::exit(1);
                            },
                        }
                        pos += 1;
                    }
                }

                instance
            }
        }
    };

    TokenStream::from(expanded)
}
