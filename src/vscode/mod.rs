use std::marker::PhantomData;

use wasm_bindgen::prelude::*;
use crate::types::*;
pub mod prelude {
    pub mod vscode {
        pub use crate::vscode::*;
    }
    pub use super::ExtensionContextApi;
}

#[wasm_bindgen(module = "vscode")]
extern "C" {
    pub type Disposable;
    pub type ExtensionContext;

    #[wasm_bindgen(method, getter, js_name = subscriptions)]
    fn subscriptions(this: &ExtensionContext) -> js_sys::Array;
}


pub trait ExtensionContextApi {
    fn subscriptions(&self) -> Mut<Array<Disposable>>;
}
impl ExtensionContextApi for ExtensionContext {
    fn subscriptions(&self) -> Mut<Array<Disposable>> {
        Mut::associate(self.as_mut(), self.subscriptions().into())
    }
}

pub mod commands {
    use std::{future::Future, process::Output, rc::Rc};

    use wasm_bindgen::prelude::*;
    use wasm_bindgen_futures::future_to_promise;

    use crate::types::IntoClosure;

    use super::Disposable;

    mod api {
        use wasm_bindgen::prelude::*;

        use crate::vscode::Disposable;
        #[wasm_bindgen(module = "vscode")]
        extern "C" {
            #[wasm_bindgen(js_namespace = commands, js_name = registerCommand)]
            pub fn register_command(message: &str, func: JsValue) -> Disposable;
        }
    }

    fn closure<F: 'static + Fn() -> JsValue>(func: F) -> Closure<dyn Fn() -> JsValue>{
        Closure::new(func)
    }

    fn async_void_closure<F: Future, T: 'static + Fn() -> F>(func: T) -> Closure<dyn Fn() -> JsValue> {
        let func = Rc::new(func);
        let func = move || {
            let func = func.clone();
            let result: JsValue = future_to_promise(async move { 
                func().await;
                Ok(JsValue::UNDEFINED)
            }).into();
            result
        };
        closure(func)
    }

    // pub fn register_command<F: Future<Output = ()>, T: 'static + Fn() -> F>(message: &str, func: T) -> Disposable {
    //     let func = async_void_closure(func);
    //     api::register_command(message, func.into_js_value())
    // }
    #[must_use]
    pub fn register_command<T: IntoClosure<(), (), S>, S>(message: &str, func: T) -> Disposable {
        // let func = func.into_closure();
        api::register_command(message, func.into_closure_value())
    }
}

pub mod window {
    use wasm_bindgen::prelude::*;

    mod api {
        use wasm_bindgen::prelude::*;

        #[wasm_bindgen(module = "vscode")]
        extern "C" {
            #[wasm_bindgen(js_namespace = window, js_name = showInformationMessage)]
            pub fn show_information_message(message: &str);

            #[wasm_bindgen(js_namespace = window, js_name = showInformationMessage, variadic)]
            pub async fn show_information_message_with_items(message: &str, items: JsValue) -> JsValue;
        }
    }
    pub fn show_information_message<Str: AsRef<str>>(message: Str) {
        api::show_information_message(message.as_ref())
    }
    pub async fn show_information_message_with_items(message: &str, items: &[&str]) -> Option<String> {
        let params = js_sys::Array::new();
        for item in items {
            params.push(&(*item).into());
        }
        let result = api::show_information_message_with_items(message, params.into()).await;
        result.as_string()
    }
}