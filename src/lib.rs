mod types;
mod vscode;
mod gen_sample;
use std::{
    future::{ready, Future, IntoFuture, Ready},
    marker::PhantomData, ops::ShlAssign,
};

use js_sys::Array;
// use vscode::ExtensionContextApi;
use vscode::prelude::*;
use wasm_bindgen::prelude::*;

async fn hello() {}

#[wasm_bindgen]
pub fn activate(context: &vscode::ExtensionContext) {
    // let x = hello;
    // let y = async {
    // 	23
    // };
    // let z = |a: usize| async {

    // };
    // let disposable = vscode::commands::register_command("trscode.helloWorld", hello);
    // vscode::commands::register_command("message", || {

    // });
    let disposable = vscode::commands::register_command("trscode.helloWorld", || async {
        // The code you place here will be executed every time your command is executed
        // Display a message box to the user
        // let resp = vscode::window::show_information_message("Hello world from rust")
        //     .items(&["hello", "world"])
        //     .await;
		let hello: JsValue = Hello { }.into();
		let world: JsValue = World { }.into();
		let undef = JsValue::UNDEFINED;
		assert!(Hello::try_from(hello.clone()).is_ok());
		assert!(!Hello::try_from(world.clone()).is_ok());
		assert!(!Hello::try_from(undef.clone()).is_ok());
		assert!(World::try_from(world.clone()).is_ok());
		assert!(!World::try_from(hello.clone()).is_ok());
		assert!(!World::try_from(undef.clone()).is_ok());
		// let hello: JsValue = hello.into();
		// let v0 = Hello::try_from(hello).is_ok();
		// let v1 = Hello::try_from(undef).is_ok();
		// let message = format!("v0.is_ok() == {v0}, v1.is_ok() == {v1}");
		vscode::window::show_information_message("Test passed!");

        // let resp = vscode::window::show_information_message_with_items(
        //     "Hello World from Rust!",
        //     &["hello", "world"],
        // )
        // .await;
        // if let Some(btn) = resp {
        //     vscode::window::show_information_message(format!("{btn} pressed!"));
        // }
    });
    context.subscriptions().push(disposable);

    let disposable = vscode::commands::register_command("trscode.helloWorldSync", || {
        // The code you place here will be executed every time your command is executed
        // Display a message box to the user
        vscode::window::show_information_message("Hello World from Rust!");
    });
    context.subscriptions().push(disposable);
}

pub trait Callable<In, Out> {
    fn call(&self, input: In) -> Out;
}

impl<Out, F: Fn() -> Out> Callable<(), Out> for F {
    fn call(&self, input: ()) -> Out {
        self()
    }
}

impl<Out, F: Fn(T0) -> Out, T0> Callable<(T0,), Out> for F {
    fn call(&self, input: (T0,)) -> Out {
        let (t0,) = input;
        self(t0)
    }
}

impl<Out, F: Fn(T0, T1) -> Out, T0, T1> Callable<(T0, T1), Out> for F {
    fn call(&self, input: (T0, T1)) -> Out {
        let (t0, t1) = input;
        self(t0, t1)
    }
}

#[must_use]
struct AsyncFnBuilder<In, Out: Future<Output = FutureOutput>, FutureOutput, Func: Callable<In, Out>>
{
    func: Func,
    args: In,
    output: PhantomData<(Out, FutureOutput)>,
}
#[must_use]
struct FnBuilder<In, Out, Func: Callable<In, Out>> {
    func: Func,
    args: In,
    output: PhantomData<Out>,
}

impl<In, Out, FutureOutput, Func> AsyncFnBuilder<In, Out, FutureOutput, Func>
where
    Out: Future<Output = FutureOutput>,
    Func: Callable<In, Out>,
{
    pub fn new(args: In, func: Func) -> Self {
        AsyncFnBuilder {
            func,
            args,
            output: PhantomData,
        }
    }
    pub fn execute(self) -> Out {
        self.func.call(self.args)
    }
}
impl<In, Out, Func: Callable<In, Out>> FnBuilder<In, Out, Func> {
    pub fn new(args: In, func: Func) -> Self {
        FnBuilder {
            func,
            args,
            output: PhantomData,
        }
    }
    pub fn execute(self) -> Out {
        self.func.call(self.args)
    }
}

type Args = (usize, Option<usize>);

impl<F: Future<Output = O>, O, T: Callable<Args, F>> AsyncFnBuilder<Args, F, O, T> {
    pub fn arg1(mut self, arg: usize) -> Self {
        self.args.1 = Some(arg);
        self
    }
}

impl<O, T: Callable<Args, O>> FnBuilder<Args, O, T> {
    pub fn another_arg(mut self, arg: usize) -> Self {
        self.args.1 = Some(arg);
        self
    }
}

impl<Args, F: Future<Output = O>, O, T: Callable<Args, F>> IntoFuture
    for AsyncFnBuilder<Args, F, O, T>
{
    type Output = O;
    type IntoFuture = F;
    fn into_future(self) -> Self::IntoFuture {
        self.execute()
    }
}
impl<Args, O, T: Callable<Args, O>> IntoFuture for FnBuilder<Args, O, T> {
    type Output = O;
    type IntoFuture = Ready<O>;
    fn into_future(self) -> Self::IntoFuture {
        ready(self.execute())
    }
}


async fn t() {
    // let func = Func { arg0: 0, arg1: Some(0) };
    // let b = Builder { func, marker: PhantomData };
    let builder = AsyncFnBuilder::new((2, None), |a, b: Option<usize>| async move {
        if let Some(b) = b {
            a + b
        } else {
            a
        }
    });
    let result = builder.arg1(23).await;
    let builder = FnBuilder::new(
        (2, None),
        |a, b: Option<usize>| {
            if let Some(b) = b {
                a + b
            } else {
                a
            }
        },
    );
    builder.another_arg(23);
    // let result = builder.another_arg(23).await;
    // let result = result.await;
}


#[wasm_bindgen]
pub struct Hello { }

#[wasm_bindgen(module = "/cast.js")]
extern "C" {
    #[wasm_bindgen(catch, js_name = cast)]
    fn try_into_hello(from: &JsValue) -> Result<Hello, JsValue>;
}

impl TryFrom<JsValue> for Hello {
	type Error = JsValue;
	fn try_from(value: JsValue) -> Result<Self, Self::Error> {
		try_into_hello(&value)
	}
}

#[wasm_bindgen]
pub struct World { }

#[wasm_bindgen(module = "/cast.js")]
extern "C" {
    #[wasm_bindgen(catch, js_name = cast)]
    fn try_into_world(from: &JsValue) -> Result<World, JsValue>;
}

impl TryFrom<JsValue> for World {
	type Error = JsValue;
	fn try_from(value: JsValue) -> Result<Self, Self::Error> {
		try_into_world(&value)
	}
}

#[wasm_bindgen(start)]
fn start() {
	
	
}