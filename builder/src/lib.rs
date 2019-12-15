extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn get_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            // can unwrap because we already checked the length
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };
    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if get_inner_type("Option", ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });
    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = get_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });
    let extend_methods = fields.iter().filter_map(|f| {
        let name = &f.ident;
        for attr in &f.attrs {
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
                if let Some(proc_macro2::TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
                    let mut tokens = g.stream().into_iter();
                    match tokens.next().unwrap() {
                        proc_macro2::TokenTree::Ident(ref i) => assert_eq!(i, "each"),
                        tt => panic!("unexpected 'each', found {}", tt),
                    }
                    match tokens.next().unwrap() {
                        proc_macro2::TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
                        tt => panic!("expected '=', found {}", tt),
                    }
                    let arg = match tokens.next().unwrap() {
                        proc_macro2::TokenTree::Literal(l) => l,
                        tt => panic!("expected literal, found {}", tt),
                    };
                    match syn::Lit::new(arg) {
                        syn::Lit::Str(s) => {
                            let arg = syn::Ident::new(&s.value(), s.span());
                            let inner_ty = get_inner_type("Vec", &f.ty).unwrap();
                            return Some(
                                quote!{ 
                                    fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                                        if let Some(ref mut values) = self.#name {
                                            values.push(#arg);
                                        } else {
                                            self.#name = Some(vec![#arg]);
                                        }
                                        self
                                    }
                                }
                            );
                        },
                        lit => panic!("expected string, found {:?}", lit),
                    }
                }
            }
        }
        None
    });
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if get_inner_type("Option", &f.ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });
    let empty_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: None
        }
    });
    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }
        impl #bident {
            #(#methods)*
            #(#extend_methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(
                    #name {
                        #(#build_fields,)*
                    }
                )
            }
        }
        impl #name {
            fn builder() -> #bident {
                #bident {
                    #(#empty_fields,)*
                }
            }
        }
    };
    expanded.into()
}
