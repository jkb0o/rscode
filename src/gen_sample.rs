use std::{collections::HashMap, marker::PhantomData};

use wasm_bindgen::{prelude::*, convert::{FromWasmAbi, IntoWasmAbi}};
use crate::types::*;

mod api {
    pub mod interface {
        pub mod command {
            use wasm_bindgen::prelude::*;
            #[wasm_bindgen]
            extern "C" {
                pub type Command;
                #[wasm_bindgen(method, getter, js_name = title)]
                pub(crate)  fn title(this: &Command) -> String;
                #[wasm_bindgen(method, getter, js_name = command)]
                pub(crate) fn command(this: &Command) -> String;
                #[wasm_bindgen(method, getter, js_name = tooltip)]
                pub(crate) fn tooltip(this: &Command) -> Option<String>;
                #[wasm_bindgen(method, getter, js_name = arguments)]
                pub(crate) fn arguments(this: &Command) -> Option<js_sys::Array>;
            }
        }
    }
}

fn x() {
    let v = JsValue::UNDEFINED;
    // let a = v.into_abi();
    // let h = unsafe { Hello::from_abi(a) };
    // v.dyn_into::<Hello>();
    // let h = String::from_abi(js)
}

pub trait ICommand: Into<JsValue> {
    fn title(&self) -> String;
    fn command(&self) -> String;
    fn tooltip(&self) -> Option<String>;
    fn arguments(&self) -> Ref<Array<JsValue>>;
}

pub use api::interface::command::Command;
impl ICommand for Command {
    fn title(&self) -> String {
        self.title()
    }
    fn command(&self) -> String {
        self.command()
    }
    fn tooltip(&self) -> Option<String> {
        self.tooltip()
    }
    fn arguments(&self) -> Ref<Array<JsValue>> {
        if let Some(args) = self.arguments() {
            Ref::associate(self, args.into())
        } else {
            Ref::associate(self, Array::new())
        }
    }
}


// defined in rscode
pub trait CodeActionsProviderImpl { }

pub trait CodeActionsProvider: Into<JsValue> + CodeActionsProviderImpl {
    fn provide_code_action(&mut self) -> usize;
    fn resolve_code_action(&mut self, action: usize);
}

// defined by user
#[wasm_bindgen]
pub struct ActionProvider {

}
// #[rscode]       (wrapper around #[wasm_bindgen])
impl CodeActionsProvider for ActionProvider {
    fn provide_code_action(&mut self) -> usize {
        23
    }
    fn resolve_code_action(&mut self, action: usize) {
        
    }
}

// generated by #[rscode] macro
impl CodeActionsProviderImpl for ActionProvider { }

#[wasm_bindgen]
impl ActionProvider {
    #[wasm_bindgen(js_name = provideCodeAction)]
    pub fn provide_code_action(&mut self) -> usize {
        <Self as CodeActionsProvider>::provide_code_action(self)
    }
    #[wasm_bindgen(js_name = resolveCodeAction)]
    pub fn resolve_code_action(&mut self, action: usize) {
        <Self as CodeActionsProvider>::resolve_code_action(self, action)
    }
}

// implemented in rscode
pub mod rscode {
    use wasm_bindgen::JsValue;

    use super::CodeActionsProvider;

    pub fn register_code_action_provider(provider: impl CodeActionsProvider) {
        let provider: JsValue = provider.into();
        
    }
}

// sample
fn test() {
    let provider = ActionProvider { };
    rscode::register_code_action_provider(provider);
}







/// Obj


pub trait ObjKey<T> {

}

pub struct Defined;
pub trait DefinedPropBuilder {
    type Output;
    fn defined(self) -> Self::Output;
}

pub struct Name<T>(T);



impl<R> DefinedPropBuilder for ObjBuilder<TerminalOptions, R> {
    type Output = ObjBuilder<TerminalOptions, R>;
    fn defined(self) -> Self::Output {
        ObjBuilder {
            data: self.data,
            marker: self.marker,
            missing: PhantomData
        }
    }
}

pub trait NamePropBuilder<T> {
    type Output;
    /// A human-readable string which will be used to represent the terminal in the UI.
    fn name<V: Into<T>>(self, value: V) -> Self::Output;
}
impl<R: Define<Name<String>>> NamePropBuilder<String> for ObjBuilder<TerminalOptions, R> {
    type Output = ObjBuilder<TerminalOptions, R::Output>;
    
    fn name<Str: Into<String>>(mut self, value: Str) -> Self::Output {
        self.data.insert("name".into(), value.into().into());
        ObjBuilder {
            data: self.data,
            marker: self.marker,
            missing: PhantomData
        }
    }
}

pub struct ShellPath<T>(T);
pub trait ShellPathPropBuilder<T> {
    type Output;
    fn shell_path<V: Into<T>>(self, value: V) -> Self::Output;
}
impl<R> ShellPathPropBuilder<String> for ObjBuilder<TerminalOptions, R> {
    type Output = Self;
    /// A path to a custom shell executable to be used in the terminal.
    fn shell_path<Str: Into<String>>(mut self, value: Str) -> Self::Output {
        self.data.insert("shellPath".into(), value.into().into());
        ObjBuilder {
            data: self.data,
            marker: self.marker,
            missing: PhantomData
        }
    }
}

pub struct Ident<T>(T);
pub trait IdentPropBuilder<T> {
    fn ident(self, value: T) -> Self;
}
impl<R, T: Into<JsValue>> IdentPropBuilder<T> for ObjBuilder<FormatingOptions<T>, R> {
    fn ident(mut self, value: T) -> Self {
        self.data.insert("ident".into(), value.into().into());
        self
    }
}

pub trait ObjKeys: Sized {
    type Required;
    // fn builder() -> ObjBuilder<Self::Required, Self>;
}


// type TerminalOptions = (Name<String>, ShellPath<String>);
pub struct TerminalOptions;
impl ObjKey<TerminalOptions> for Defined { }
impl ObjKey<TerminalOptions> for Name<String> { }
impl ObjKey<TerminalOptions> for ShellPath<String> { }
pub struct FormatingOptions<T>(PhantomData<T>);
impl<T: Into<JsValue>> ObjKey<FormatingOptions<T>> for Ident<T> { }
pub struct IntOptions;
impl ObjKey<Name<i32>> for Name<i32> { }
impl ObjKeys for IntOptions {
    type Required = (Name<i32>,);
}


impl ObjKeys for TerminalOptions {
    type Required = (Name<String>,);
    // fn builder() -> ObjBuilder<Self::Required, Self> {
    //     ObjBuilder {
    //         data: HashMap::new(),
    //         marker: PhantomData,
    //         missing: PhantomData::<(Name<String>,)>,
    //     }
    // }
}

impl<T: Into<JsValue>> ObjKeys for FormatingOptions<T> {
    type Required = ();
    // fn builder() -> ObjBuilder<Self::Required, Self> {
    //     ObjBuilder {
    //         data: HashMap::new(),
    //         marker: PhantomData,
    //         missing: PhantomData::<()>,
    //     }
    // }
}

pub struct Obj<T: ObjKeys>{
    data: HashMap<String, JsValue>,
    marker: PhantomData<T>
}
impl<T: ObjKeys> Obj<T> {
    pub fn b() -> ObjBuilder<T, T::Required> {
        ObjBuilder::new()
    }
    pub fn empty() -> Self {
        Obj {
            data: HashMap::new(),
            marker: PhantomData
        }
    }
}



pub struct ObjBuilder<T: ObjKeys, R> {
    data: HashMap<String, JsValue>,
    marker: PhantomData<T>,
    missing: PhantomData<R>,
}

// pub struct OBuilder<R, T: ObjKeys> {
//     data: HashMap<String, JsValue>,
//     marker: PhantomData<T>,
//     required: T::Required
// }


impl<T: ObjKeys> ObjBuilder<T, <T as ObjKeys>::Required> 
// where
//     R: <T as ObjKeys>::Required
{
    pub fn new() -> Self {
        Self { 
            data: HashMap::new(),
            marker: PhantomData,
            missing: Default::default()
        }
    }
}


trait Valid { }
trait Invalid { }

impl Valid for () { }
impl<R> Invalid for (R,) { }
impl<R0, R1> Invalid for (R0,R1) { }

impl<T: ObjKeys, R> ObjBuilder<T, R> {
    pub fn build(&self) -> Obj<T> {
        Obj {
            // data: self.data,
            data: HashMap::new(),
            marker: self.marker
        }
    }
}
// impl<T: ObjKeys> ObjBuilder<T, (Name<String>,)> {
//     pub fn build(&self) -> Obj<T> {
//         Obj {
//             // data: self.data,
//             data: HashMap::new(),
//             marker: self.marker
//         }
//     }
// }

pub trait ValidObjBuilder<T: ObjKeys> {
    fn build(&self) -> Obj<T> {
        Obj {
            // data: self.data,
            data: HashMap::new(),
            marker: PhantomData
        }
    }
}
// impl<T: ObjKeys> ValidObjBuilder<T> for &ObjBuilder<T, ()> { }


pub trait InvalidObjBuilder<T: ObjKeys> {
    #[deprecated( note = "Missing required key(s)")]
    fn build(&self) -> Obj<T> {
        Obj {
            // data: self.data,
            data: HashMap::new(),
            marker: PhantomData
        }
    }
}

// impl<R0, T: ObjKeys> InvalidObjBuilder<T> for &ObjBuilder<T, (R0,)> { }
// impl<R0, R1, T: ObjKeys> InvalidObjBuilder<T> for &ObjBuilder<T, (R0, R1)> { }

pub struct Undefined<T>(PhantomData<T>);
impl<T> From<PhantomData<T>> for Undefined<T> {
    fn from(value: PhantomData<T>) -> Self {
        Undefined(PhantomData)
    }
}
impl Undefined<()> {
    pub fn build<O, F: FnOnce() -> O>(&self, build: F) -> O {
        build()
    }
}

pub trait Define<T> { 
    type Output;
}
impl Define<Name<String>> for (Name<String>,) {
    type Output = ();
}
impl Define<Name<String>> for (Name<String>, ShellPath<String>) {
    type Output = (ShellPath<String>,);
}
impl Define<ShellPath<String>> for (ShellPath<String>,) {
    type Output = ();
}
impl Define<ShellPath<String>> for (Name<String>, ShellPath<String>) {
    type Output = (Name<String>,);
}


// fn test_define() {
//     let required: PhantomData<(Name<String>, ShellPath<String>)> = PhantomData;
//     let r0 = required.define(PhantomData::<Name<String>>);
//     let r1 = required.define(PhantomData::<ShellPath<String>>);
//     let r2 = r0.define(PhantomData::<ShellPath<String>>);
//     let r3 = r1.define(PhantomData::<Name<String>>);
// }

pub fn validator<R, T: ObjKeys>(builder: &ObjBuilder<T, R>) -> Undefined<R> {
    builder.missing.into()
}


macro_rules! obj {
    

    (@arg($obj:expr) $name:ident: $val:expr, $last:ident) => {
        @build $obj.$name($val).$last(InvalidValue)
    };

    (@arg($obj:expr) $name:ident: $val:expr$(,)?) => {
        $obj.$name($val)
    };
    
    (@arg($obj:expr) $name:ident: $val:expr, $($rest:tt)*) => {
        @arg($obj.$name($val)) $($rest)*
    };
    (@build $builder:expr) => {
        // $builder.build()
        validator(&$builder).build(|| $builder.build())
    };
    // ($($name:ident: $val:expr,)* $rest:ident) => {
    //     ObjBuilder::<TerminalOptions>::new()
    //         .$rest("")
    //         .build()
    // };

    ($name:ident: $val:expr, $last:ident) => {
        obj!(@build ObjBuilder::new().$name($val).$last(
            compile_error!("Expected value.")
        ))
    };
    ($name:ident: $val:expr$(,)?) => {
        obj!(@build ObjBuilder::new().$name($val))
    };
    
    ($name:ident: $val:expr, $($rest:tt)*) => {
        obj!(@build obj!(@arg(ObjBuilder::new().$name($val)) $($rest)*))
    };
    ($name:ident) => {
        obj!(@build ObjBuilder::new().$name(
            compile_error!("Expected value.")
        ))
    };
    () => {
        // compile_error!("Empty obj!{ } not supported")
        obj!(@build ObjBuilder::new().defined())
        // Obj::empty()
    };

}

fn test_obj() {
    // let builder: ObjBuilder<_, TerminalOptions> = ObjBuilder::new();
    // let b = builder.name("hello");
    // b.
    let b: ObjBuilder<TerminalOptions, _> = ObjBuilder::new();
    let o: Obj<TerminalOptions> = {
        let b = ObjBuilder::new().defined();
        // let b = Obj::b();
        // Obj::build(b)
        // let b = b.typed();
        let b = (&b).build();
        b
    };
    // let obj: Obj<TerminalOptions> = ObjKeys::builder().build();
    // let obj: Obj<TerminalOptions> = ObjKeys::builder()
    //     .name("hello")
    //     .shell_path("hello")
    //     .build();
        

    // let obj: Obj<TerminalOptions> = obj! {
    //     name: "hello",
    //     shell_path: "world"
    // };
    add_opts(obj! {
        shell_path: "hello",
        name: "hello"
    });
    

    let _x = format(obj! {
        ident: "hello!"
        // ident: 23
    });

    // let obj: Option<Obj<TerminalOptions>> = obj!{
    //     name

        
           
    // };
    // obj!(@arg name: "bob"; shell_path: "hello";);

    let u = takes_union(obj! {
        ident: "hello"
    });
    let x = takes_union::<String, _>(obj! {
        name: "hello"
    });

    let x = takes_union2("opts".to_string());
    let x = takes_union2(obj! {
        name: "world"
    });
    let x = takes_union2("opts".to_string());
    // let x = takes_union2("opts");
}
pub enum Void { }

fn add_opts(ots: Obj<TerminalOptions>) {


}

fn format<T: Into<JsValue>>(opts: Obj<FormatingOptions<T>>) -> FormatingOptions<T> {
    FormatingOptions(PhantomData)

}

fn takes_union<O: Into<JsValue>, T: InUnion<(Obj<FormatingOptions<O>>, Obj<TerminalOptions>)>>(value: T) -> T {
    value

}

fn takes_union2<T: InUnion<(String, Obj<TerminalOptions>)>>(opts: T) -> T { 
    opts
}




impl<T: Into<JsValue>> InUnion<(Obj<FormatingOptions<T>>, Obj<TerminalOptions>)> for Obj<TerminalOptions> { }
impl<T: Into<JsValue>> InUnion<(Obj<FormatingOptions<T>>, Obj<TerminalOptions>)> for Obj<FormatingOptions<T>> { }

impl InUnion<(String, Obj<TerminalOptions>)> for String { }
impl InUnion<(String, Obj<TerminalOptions>)> for Obj<TerminalOptions> { }