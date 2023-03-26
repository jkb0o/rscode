use std::{marker::PhantomData, future::Future, rc::Rc, any::{TypeId, type_name}};

use wasm_bindgen::{JsValue, prelude::Closure, convert::IntoWasmAbi};
use wasm_bindgen_futures::future_to_promise;

pub trait JsVal: From<JsValue> + Into<JsValue> { }
impl<T: From<JsValue> + Into<JsValue>> JsVal for T { }

pub trait TryFromJsValue: Sized {
    fn try_from_jsvalue(value: JsValue) -> Option<Self>;
}

impl TryFromJsValue for String {
    fn try_from_jsvalue(value: JsValue) -> Option<Self> {
        value.as_string()
    }
}
pub trait MutateJsVal {
    fn as_mut(&self) -> &mut Self;
}

impl<T: JsVal> MutateJsVal for T {
    fn as_mut(&self) -> &mut Self {
        let this = self as *const Self;
        let this = this as *mut Self;
        unsafe { this.as_mut().unwrap() }
    }
}

pub trait IntoJsValue<T> {
    fn into_js_value(self) -> JsValue;
}

impl<T: Into<JsValue>> IntoJsValue<T> for T {
    fn into_js_value(self) -> JsValue {
        self.into()
    }
}

impl<'a, T: Into<JsValue>> IntoJsValue<T> for Ref<'a, T> {
    fn into_js_value(self) -> JsValue {
        self.0.into()
    }
}

pub struct Array<T: JsVal>(js_sys::Array, PhantomData<T>);

impl<T: JsVal> Array<T> {

    pub fn new() -> Self {
        Self(js_sys::Array::new(), PhantomData)
    }

    /// The `push()` method adds one or more elements to the end of an array and
    /// returns the new length of the array.
    ///
    /// [MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push)
    pub fn push<V: IntoJsValue<T>>(&mut self, value: V) -> usize {
        self.0.push(&value.into_js_value()) as usize
    }

    /// The `pop()` method removes the last element from an array and returns that
    /// element. This method changes the length of the array.
    ///
    /// [MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop)
    pub fn pop(&self) -> Option<Ref<T>> {
        if self.0.length() == 0 {
            return None;
        }
        Some(Ref::associate(self, self.0.pop().into()))
    }
}

impl<T: JsVal> From<js_sys::Array> for Array<T> {
    fn from(value: js_sys::Array) -> Self {
        Array(value, PhantomData)
    }
}

// impl<T: From<JsValue> + Into<JsValue>> std::ops::Index<usize> for Array<T> {
//     type Output = Ref<T>;
//     fn index(&self, index: usize) -> &Self::Output {
//         &self.0.at(index as i32).into()
//     }
// }

// impl<T: From<JsValue> + Into<JsValue>> std::ops::IndexMut<usize> for Array<T> {
//     fn index_mut(&mut self, index: usize) -> &mut Self::Output {
//         &mut self.0.at(index as i32).into()
//     }
// }


pub struct Map<K: JsVal, V: JsVal>(js_sys::Map, PhantomData<(K, V)>);
impl <K: JsVal, V: JsVal> Map<K, V> {
    pub fn from<TK: Into<K>, TV: Into<V>>(slice: &[(TK, TV)]) -> Self {
        let this = Self(js_sys::Map::new(), PhantomData);
        this
    }
}

pub struct Ref<'a, T>(T, PhantomData<&'a ()>);

impl<'a, T> Ref<'a, T> {
    pub fn associate<S>(_: &'a S, value: T) -> Self {
        Self(value, PhantomData)
    }
}

impl<'a, T> std::ops::Deref for Ref<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Mut<'a, T>(T, PhantomData<&'a mut ()>);

impl<'a, T> Mut<'a, T> {
    pub fn associate<S>(_: &'a mut S, value: T) -> Self {
        Self(value, PhantomData)
    }
}

impl<'a, T> std::ops::Deref for Mut<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'a, T> std::ops::DerefMut for Mut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


// impl<'a, T> Into<T> for Ref<'a, T> {
//     fn into(self) -> T {
//         self.0
//     }
// }

// impl<'a, T> From<Ref<'a, T>> for T {
//     fn from(value: Ref<'a, T>) -> Self {
//         value.0
//     }
// }

// impl std::ops::Deref

pub struct Sync;
pub struct SyncVoid;
pub struct Async;
pub struct AsyncVoid;

pub trait IntoClosure<In, Out, S> {
    type Closure;
    fn into_closure(self) -> Self::Closure;
    fn into_closure_value(self) -> JsValue;
}

impl<C: 'static + Fn()> IntoClosure<(), (), SyncVoid> for C {
    type Closure = Closure<dyn Fn()>;
    fn into_closure(self) -> Self::Closure {
        Closure::new(self)
    }
    fn into_closure_value(self) -> JsValue {
        self.into_closure().into_js_value()
    }
}
impl<R: Into<JsValue> + Sized + 'static + IntoWasmAbi, C: 'static + Fn() -> R> IntoClosure<(), R, Sync> for C {
    type Closure = Closure<dyn Fn() -> R>;
    fn into_closure(self) -> Self::Closure {
        Closure::new(self)
    }
    fn into_closure_value(self) -> JsValue {
        self.into_closure().into_js_value()
    }
}

impl<F: Future<Output = ()>, C: 'static + Fn() -> F> IntoClosure<(), (), AsyncVoid> for C {
    type Closure = Closure<dyn Fn() -> JsValue>;
    fn into_closure(self) -> Self::Closure {
        let func = Rc::new(self);
        let func = move || {
            let func = func.clone();
            let result: JsValue = future_to_promise(async move { 
                func().await;
                Ok(JsValue::UNDEFINED)
            }).into();
            result
        };
        func.into_closure()
    }
    fn into_closure_value(self) -> JsValue {
        self.into_closure().into_js_value()
    }
}

impl<R: Into<JsValue> + Sized, F: Future<Output = R>, C: 'static + Fn() -> F> IntoClosure<(), R, Async> for C {
    type Closure = Closure<dyn Fn() -> JsValue>;
    fn into_closure(self) -> Self::Closure {
        let func = Rc::new(self);
        let func = move || {
            let func = func.clone();
            let result: JsValue = future_to_promise(async move { 
                func().await;
                Ok(JsValue::UNDEFINED)
            }).into();
            result
        };
        func.into_closure()
    }
    fn into_closure_value(self) -> JsValue {
        self.into_closure().into_js_value()
    }
}

pub trait Func<In, Out> {
    fn into_js_func(self) -> JsValue;
}

fn register_command<S, F: IntoClosure<(), (), S>>(func: F) {
    let c = func.into_closure();
}

use wasm_bindgen::prelude::*;
// #[wasm_bindgen]
// pub struct Response {
//     pub size: usize
// }
#[wasm_bindgen]
pub struct Response(usize);
//     contents: u32,
// }

fn authenticate<S, F: IntoClosure<(), Response, S>>(func: F) {

}

fn invoke() {
    register_command(|| {

    });
    register_command(|| async {

    });
    authenticate(|| Response(2));
    authenticate(|| async { Response(2) });
    let x = async {

    };
    
}

struct Undefined;
impl From<Undefined> for JsValue {
    fn from(_: Undefined) -> Self {
        JsValue::undefined()
    }
}

pub struct Union<T>(JsValue, TypeId, PhantomData<T>);

pub trait InUnion<T> {}

impl<T> Union<T> {
    fn is<V: InUnion<T> + 'static>(&self) -> bool {
        self.1 == TypeId::of::<V>()
    }
    fn into<V: InUnion<T> + From<JsValue> + 'static>(self) -> V {
        self.to()
    }
    fn to<V: InUnion<T> + From<JsValue> + 'static>(&self) -> V {
        if self.is::<V>() {
            self.0.clone().into()
        } else {
            panic!("Union member is not of type {}", type_name::<V>())
        }
    }
    fn cast<V: InUnion<T> + From<JsValue> + 'static>(&self) -> Option<V> {
        if self.is::<V>() {
            Some(self.0.clone().into())
        } else {
            None
        }
    }
}

impl<T> From<Union<T>> for JsValue {
    fn from(value: Union<T>) -> Self {
        value.0
    }
}

impl<T0> Default for Union<(Undefined, T0)> {
    fn default() -> Self {
        Union(JsValue::undefined(), TypeId::of::<Undefined>(), PhantomData)
    }
}
impl<T0, T1> Default for Union<(Undefined, T0, T1)> {
    fn default() -> Self {
        Union(JsValue::undefined(), TypeId::of::<Undefined>(), PhantomData)
    }
}
impl<T0, T1, T2> Default for Union<(Undefined, T0, T1, T2)> {
    fn default() -> Self {
        Union(JsValue::undefined(),  TypeId::of::<Undefined>(), PhantomData)
    }
}


impl InUnion<(Undefined, bool, String)> for String { }
impl From<String> for Union<(Undefined, bool, String)> {
    fn from(value: String) -> Self {
        Union(value.into(),  TypeId::of::<(String, &str)>(), PhantomData)
    }
}

impl InUnion<(Undefined, bool, String)> for &str { }
impl From<&str> for Union<(Undefined, bool, String)> {
    fn from(value: &str) -> Self {
        Union(value.into(), TypeId::of::<(String, &str)>(), PhantomData)
    }
}

impl InUnion<(Undefined, bool, String)> for bool { }
impl From<bool> for Union<(Undefined, bool, String)> {
    fn from(value: bool) -> Self {
        Union(value.into(), TypeId::of::<bool>(),  PhantomData)
    }
}

impl InUnion<(Undefined, f32, String)> for String { }
impl From<String> for Union<(Undefined, f32, String)> {
    fn from(value: String) -> Self {
        Union(value.into(), TypeId::of::<(String, &str)>(), PhantomData)
    }
}

impl InUnion<(Undefined, f32, String)> for &str { }
impl From<&str> for Union<(Undefined, f32, String)> {
    fn from(value: &str) -> Self {
        Union(value.into(), TypeId::of::<(String, &str)>(), PhantomData)
    }
}

impl InUnion<(Undefined, f32, String)> for f32 { }
impl From<f32> for Union<(Undefined, f32, String)> {
    fn from(value: f32) -> Self {
        Union(value.into(), TypeId::of::<f32>(), PhantomData)
    }
}

#[derive(Default)]
struct TextEditorOptions {
    tab_size: Union<(Undefined, f32, String)>,
    insert_spaces: Union<(Undefined, bool, String)>,
}

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
fn test_union() {
    // js_sys::Map
    let opts = TextEditorOptions {
        insert_spaces: false.into(),
        ..Default::default()
    };
    let x = opts.insert_spaces.is::<bool>();
    
    let i: Option<usize> = 23.into();
    let s: JsValue = "hello".into();
    // s.
    // let v = s.dyn_into::<String>().unwrap();
    // let v: String = String::try_from(s).unwrap();
    // s.
    // let m: Map<&'static str, Vec<String>> = Map::from(&[
    //     ("Images", vec!["hello".into()])
    // ]);
}

