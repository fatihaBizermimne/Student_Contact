(function (Global) {
    "use strict";

    // Polyfill

    if (!Date.now) {
        Date.now = function () {
            return new Date().getTime();
        };
    }

    if (!Math.trunc) {
        Math.trunc = function (x) {
            return x < 0 ? Math.ceil(x) : Math.floor(x);
        }
    }

    if (!Object.setPrototypeOf) {
        Object.setPrototypeOf = function (obj, proto) {
            obj.__proto__ = proto;
            return obj;
        }
    }

    Global.WebSharper = {
        Runtime: {
            Ctor: function (ctor, typeFunction) {
                ctor.prototype = typeFunction.prototype;
                return ctor;
            },

            Class: function (members, base, statics) {
                var proto = members;
                if (base) {
                    proto = new base();
                    for (var m in members) { proto[m] = members[m] }
                }
                var typeFunction = function (copyFrom) {
                    if (copyFrom) {
                        for (var f in copyFrom) { this[f] = copyFrom[f] }
                    }
                }
                typeFunction.prototype = proto;
                if (statics) {
                    for (var f in statics) { typeFunction[f] = statics[f] }
                }
                return typeFunction;
            },

            Clone: function (obj) {
                var res = {};
                for (var p of Object.getOwnPropertyNames(obj)) { res[p] = obj[p] }
                Object.setPrototypeOf(res, Object.getPrototypeOf(obj));
                return res;
            },

            NewObject:
                function (kv) {
                    var o = {};
                    for (var i = 0; i < kv.length; i++) {
                        o[kv[i][0]] = kv[i][1];
                    }
                    return o;
                },

            PrintObject:
                function (obj) {
                    var res = "{ ";
                    var empty = true;
                    for (var field of Object.getOwnPropertyNames(obj)) {
                        if (empty) {
                            empty = false;
                        } else {
                            res += ", ";
                        }
                        res += field + " = " + obj[field];
                    }
                    if (empty) {
                        res += "}";
                    } else {
                        res += " }";
                    }
                    return res;
                },

            DeleteEmptyFields:
                function (obj, fields) {
                    for (var i = 0; i < fields.length; i++) {
                        var f = fields[i];
                        if (obj[f] === void (0)) { delete obj[f]; }
                    }
                    return obj;
                },

            GetOptional:
                function (value) {
                    return (value === void (0)) ? null : { $: 1, $0: value };
                },

            SetOptional:
                function (obj, field, value) {
                    if (value) {
                        obj[field] = value.$0;
                    } else {
                        delete obj[field];
                    }
                },

            SetOrDelete:
                function (obj, field, value) {
                    if (value === void (0)) {
                        delete obj[field];
                    } else {
                        obj[field] = value;
                    }
                },

            Apply: function (f, obj, args) {
                return f.apply(obj, args);
            },

            Bind: function (f, obj) {
                return function () { return f.apply(this, arguments) };
            },

            CreateFuncWithArgs: function (f) {
                return function () { return f(Array.prototype.slice.call(arguments)) };
            },

            CreateFuncWithOnlyThis: function (f) {
                return function () { return f(this) };
            },

            CreateFuncWithThis: function (f) {
                return function () { return f(this).apply(null, arguments) };
            },

            CreateFuncWithThisArgs: function (f) {
                return function () { return f(this)(Array.prototype.slice.call(arguments)) };
            },

            CreateFuncWithRest: function (length, f) {
                return function () { return f(Array.prototype.slice.call(arguments, 0, length).concat([Array.prototype.slice.call(arguments, length)])) };
            },

            CreateFuncWithArgsRest: function (length, f) {
                return function () { return f([Array.prototype.slice.call(arguments, 0, length), Array.prototype.slice.call(arguments, length)]) };
            },

            BindDelegate: function (func, obj) {
                var res = func.bind(obj);
                res.$Func = func;
                res.$Target = obj;
                return res;
            },

            CreateDelegate: function (invokes) {
                if (invokes.length == 0) return null;
                if (invokes.length == 1) return invokes[0];
                var del = function () {
                    var res;
                    for (var i = 0; i < invokes.length; i++) {
                        res = invokes[i].apply(null, arguments);
                    }
                    return res;
                };
                del.$Invokes = invokes;
                return del;
            },

            CombineDelegates: function (dels) {
                var invokes = [];
                for (var i = 0; i < dels.length; i++) {
                    var del = dels[i];
                    if (del) {
                        if ("$Invokes" in del)
                            invokes = invokes.concat(del.$Invokes);
                        else
                            invokes.push(del);
                    }
                }
                return WebSharper.Runtime.CreateDelegate(invokes);
            },

            DelegateEqual: function (d1, d2) {
                if (d1 === d2) return true;
                if (d1 == null || d2 == null) return false;
                var i1 = d1.$Invokes || [d1];
                var i2 = d2.$Invokes || [d2];
                if (i1.length != i2.length) return false;
                for (var i = 0; i < i1.length; i++) {
                    var e1 = i1[i];
                    var e2 = i2[i];
                    if (!(e1 === e2 || ("$Func" in e1 && "$Func" in e2 && e1.$Func === e2.$Func && e1.$Target == e2.$Target)))
                        return false;
                }
                return true;
            },

            ThisFunc: function (d) {
                return function () {
                    var args = Array.prototype.slice.call(arguments);
                    args.unshift(this);
                    return d.apply(null, args);
                };
            },

            ThisFuncOut: function (f) {
                return function () {
                    var args = Array.prototype.slice.call(arguments);
                    return f.apply(args.shift(), args);
                };
            },

            ParamsFunc: function (length, d) {
                return function () {
                    var args = Array.prototype.slice.call(arguments);
                    return d.apply(null, args.slice(0, length).concat([args.slice(length)]));
                };
            },

            ParamsFuncOut: function (length, f) {
                return function () {
                    var args = Array.prototype.slice.call(arguments);
                    return f.apply(null, args.slice(0, length).concat(args[length]));
                };
            },

            ThisParamsFunc: function (length, d) {
                return function () {
                    var args = Array.prototype.slice.call(arguments);
                    args.unshift(this);
                    return d.apply(null, args.slice(0, length + 1).concat([args.slice(length + 1)]));
                };
            },

            ThisParamsFuncOut: function (length, f) {
                return function () {
                    var args = Array.prototype.slice.call(arguments);
                    return f.apply(args.shift(), args.slice(0, length).concat(args[length]));
                };
            },

            Curried: function (f, n, args) {
                args = args || [];
                return function (a) {
                    var allArgs = args.concat([a === void (0) ? null : a]);
                    if (n == 1)
                        return f.apply(null, allArgs);
                    if (n == 2)
                        return function (a) { return f.apply(null, allArgs.concat([a === void (0) ? null : a])); }
                    return WebSharper.Runtime.Curried(f, n - 1, allArgs);
                }
            },

            Curried2: function (f) {
                return function (a) { return function (b) { return f(a, b); } }
            },

            Curried3: function (f) {
                return function (a) { return function (b) { return function (c) { return f(a, b, c); } } }
            },

            UnionByType: function (types, value, optional) {
                var vt = typeof value;
                for (var i = 0; i < types.length; i++) {
                    var t = types[i];
                    if (typeof t == "number") {
                        if (Array.isArray(value) && (t == 0 || value.length == t)) {
                            return { $: i, $0: value };
                        }
                    } else {
                        if (t == vt) {
                            return { $: i, $0: value };
                        }
                    }
                }
                if (!optional) {
                    throw new Error("Type not expected for creating Choice value.");
                }
            },

            MarkResizable: function (arr) {
                Object.defineProperty(arr, "resizable", { enumerable: false, writable: false, configurable: false, value: true });
                return arr;
            },

            MarkReadOnly: function (arr) {
                Object.defineProperty(arr, "readonly", { enumerable: false, writable: false, configurable: false, value: true });
                return arr;
            },

            ScriptBasePath: "./",

            ScriptPath: function (a, f) {
                return this.ScriptBasePath + (this.ScriptSkipAssemblyDir ? "" : a + "/") + f;
            },

            OnLoad:
                function (f) {
                    if (!("load" in this)) {
                        this.load = [];
                    }
                    this.load.push(f);
                },

            Start:
                function () {
                    function run(c) {
                        for (var i = 0; i < c.length; i++) {
                            c[i]();
                        }
                    }
                    if ("load" in this) {
                        run(this.load);
                        this.load = [];
                    }
                },
        }
    }

    Global.WebSharper.Runtime.OnLoad(function () {
        if (Global.WebSharper && WebSharper.Activator && WebSharper.Activator.Activate)
            WebSharper.Activator.Activate()
    });

    Global.ignore = function () { };
    Global.id = function (x) { return x };
    Global.fst = function (x) { return x[0] };
    Global.snd = function (x) { return x[1] };
    Global.trd = function (x) { return x[2] };

    if (!Global.console) {
        Global.console = {
            count: ignore,
            dir: ignore,
            error: ignore,
            group: ignore,
            groupEnd: ignore,
            info: ignore,
            log: ignore,
            profile: ignore,
            profileEnd: ignore,
            time: ignore,
            timeEnd: ignore,
            trace: ignore,
            warn: ignore
        }
    }
}(self));
;
/* https://github.com/jonathantneal/closest */
(function (w, p) {
    p = w.Element.prototype
    if (!p.matches) { p.matches = p.msMatchesSelector || function (s) { var m = (this.document || this.ownerDocument).querySelectorAll(s); for (var i = 0; m[i] && m[i] !== e; ++i); return !!m[i] } }
    if (!p.closest) { p.closest = function (s) { var e = this; while (e && e.nodeType == 1) { if (e.matches(s)) return e; e = e.parentNode } return null } }
})(self);
(function () {
    var lastTime = 0;
    var vendors = ['webkit', 'moz'];
    for (var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
        window.requestAnimationFrame = window[vendors[x] + 'RequestAnimationFrame'];
        window.cancelAnimationFrame =
            window[vendors[x] + 'CancelAnimationFrame'] || window[vendors[x] + 'CancelRequestAnimationFrame'];
    }

    if (!window.requestAnimationFrame)
        window.requestAnimationFrame = function (callback, element) {
            var currTime = new Date().getTime();
            var timeToCall = Math.max(0, 16 - (currTime - lastTime));
            var id = window.setTimeout(function () { callback(currTime + timeToCall); },
                timeToCall);
            lastTime = currTime + timeToCall;
            return id;
        };

    if (!window.cancelAnimationFrame)
        window.cancelAnimationFrame = function (id) {
            clearTimeout(id);
        };
}());
;
(function (Global) {
    "use strict";
    var ProjectWeb, ContactManager, Contact, WebSharper, Operators, Forms, Form, Obj, UI, Var, List, T, JavaScript, JS, SC$1, Pervasives, Doc, View, HtmlModule, attr, View$1, Submitter, Utils, Client, Templates, Pervasives$1, Validation, Result, Var$1, Snap, AttrProxy, Seq, ErrorMessage, Unchecked, EventTarget, Node, Collections, Dictionary, Fresh, Object, ConcreteVar, Docs, Attrs, DomUtility, AttrModule, Array, Enumerator, T$1, Arrays, SC$2, HashSet, WindowOrWorkerGlobalScope, Abbrev, Fresh$1, DocElemNode, CharacterData, BindVar, Elt, DictionaryUtil, Element, Prepare, Slice, KeyCollection, Numeric, An, Settings, Mailbox, SC$3, Attrs$1, Dyn, SC$4, Updates, Strings, Docs$1, RunState, NodeSet, Concurrency, Anims, SC$5, SC$6, Queue, String, CheckedInput, SC$7, SC$8, AppendList, HashSetUtil, DynamicAttrNode, Char, DateUtil, Error, KeyNotFoundException, Scheduler, Easing, AsyncBody, SC$9, CT, HashSet$1, CancellationTokenSource, DomNodes, OperationCanceledException, Lazy, SC$10, LazyExtensionsProxy, LazyRecord, Runtime, console, Date;
    ProjectWeb = Global.ProjectWeb = Global.ProjectWeb || {};
    ContactManager = ProjectWeb.ContactManager = ProjectWeb.ContactManager || {};
    Contact = ContactManager.Contact = ContactManager.Contact || {};
    WebSharper = Global.WebSharper = Global.WebSharper || {};
    Operators = WebSharper.Operators = WebSharper.Operators || {};
    Forms = WebSharper.Forms = WebSharper.Forms || {};
    Form = Forms.Form = Forms.Form || {};
    Obj = WebSharper.Obj = WebSharper.Obj || {};
    UI = WebSharper.UI = WebSharper.UI || {};
    Var = UI.Var = UI.Var || {};
    List = WebSharper.List = WebSharper.List || {};
    T = List.T = List.T || {};
    JavaScript = WebSharper.JavaScript = WebSharper.JavaScript || {};
    JS = JavaScript.JS = JavaScript.JS || {};
    SC$1 = Global.StartupCode$ProjectWeb$Client = Global.StartupCode$ProjectWeb$Client || {};
    Pervasives = JavaScript.Pervasives = JavaScript.Pervasives || {};
    Doc = UI.Doc = UI.Doc || {};
    View = UI.View = UI.View || {};
    HtmlModule = UI.HtmlModule = UI.HtmlModule || {};
    attr = HtmlModule.attr = HtmlModule.attr || {};
    View$1 = Forms.View = Forms.View || {};
    Submitter = UI.Submitter = UI.Submitter || {};
    Utils = WebSharper.Utils = WebSharper.Utils || {};
    Client = UI.Client = UI.Client || {};
    Templates = Client.Templates = Client.Templates || {};
    Pervasives$1 = Forms.Pervasives = Forms.Pervasives || {};
    Validation = Forms.Validation = Forms.Validation || {};
    Result = Forms.Result = Forms.Result || {};
    Var$1 = UI.Var$1 = UI.Var$1 || {};
    Snap = UI.Snap = UI.Snap || {};
    AttrProxy = UI.AttrProxy = UI.AttrProxy || {};
    Seq = WebSharper.Seq = WebSharper.Seq || {};
    ErrorMessage = Forms.ErrorMessage = Forms.ErrorMessage || {};
    Unchecked = WebSharper.Unchecked = WebSharper.Unchecked || {};
    EventTarget = Global.EventTarget;
    Node = Global.Node;
    Collections = WebSharper.Collections = WebSharper.Collections || {};
    Dictionary = Collections.Dictionary = Collections.Dictionary || {};
    Fresh = Forms.Fresh = Forms.Fresh || {};
    Object = Global.Object;
    ConcreteVar = UI.ConcreteVar = UI.ConcreteVar || {};
    Docs = UI.Docs = UI.Docs || {};
    Attrs = UI.Attrs = UI.Attrs || {};
    DomUtility = UI.DomUtility = UI.DomUtility || {};
    AttrModule = UI.AttrModule = UI.AttrModule || {};
    Array = UI.Array = UI.Array || {};
    Enumerator = WebSharper.Enumerator = WebSharper.Enumerator || {};
    T$1 = Enumerator.T = Enumerator.T || {};
    Arrays = WebSharper.Arrays = WebSharper.Arrays || {};
    SC$2 = Global.StartupCode$WebSharper_UI$Templates = Global.StartupCode$WebSharper_UI$Templates || {};
    HashSet = Collections.HashSet = Collections.HashSet || {};
    WindowOrWorkerGlobalScope = Global.WindowOrWorkerGlobalScope;
    Abbrev = UI.Abbrev = UI.Abbrev || {};
    Fresh$1 = Abbrev.Fresh = Abbrev.Fresh || {};
    DocElemNode = UI.DocElemNode = UI.DocElemNode || {};
    CharacterData = Global.CharacterData;
    BindVar = UI.BindVar = UI.BindVar || {};
    Elt = UI.Elt = UI.Elt || {};
    DictionaryUtil = Collections.DictionaryUtil = Collections.DictionaryUtil || {};
    Element = Global.Element;
    Prepare = Templates.Prepare = Templates.Prepare || {};
    Slice = WebSharper.Slice = WebSharper.Slice || {};
    KeyCollection = Collections.KeyCollection = Collections.KeyCollection || {};
    Numeric = WebSharper.Numeric = WebSharper.Numeric || {};
    An = UI.An = UI.An || {};
    Settings = Client.Settings = Client.Settings || {};
    Mailbox = Abbrev.Mailbox = Abbrev.Mailbox || {};
    SC$3 = Global.StartupCode$WebSharper_Forms$Forms = Global.StartupCode$WebSharper_Forms$Forms || {};
    Attrs$1 = Client.Attrs = Client.Attrs || {};
    Dyn = Attrs$1.Dyn = Attrs$1.Dyn || {};
    SC$4 = Global.StartupCode$WebSharper_UI$Attr_Client = Global.StartupCode$WebSharper_UI$Attr_Client || {};
    Updates = UI.Updates = UI.Updates || {};
    Strings = WebSharper.Strings = WebSharper.Strings || {};
    Docs$1 = Client.Docs = Client.Docs || {};
    RunState = Docs$1.RunState = Docs$1.RunState || {};
    NodeSet = Docs$1.NodeSet = Docs$1.NodeSet || {};
    Concurrency = WebSharper.Concurrency = WebSharper.Concurrency || {};
    Anims = UI.Anims = UI.Anims || {};
    SC$5 = Global.StartupCode$WebSharper_UI$Doc_Proxy = Global.StartupCode$WebSharper_UI$Doc_Proxy || {};
    SC$6 = Global.StartupCode$WebSharper_UI$Abbrev = Global.StartupCode$WebSharper_UI$Abbrev || {};
    Queue = WebSharper.Queue = WebSharper.Queue || {};
    String = UI.String = UI.String || {};
    CheckedInput = UI.CheckedInput = UI.CheckedInput || {};
    SC$7 = Global.StartupCode$WebSharper_UI$DomUtility = Global.StartupCode$WebSharper_UI$DomUtility || {};
    SC$8 = Global.StartupCode$WebSharper_UI$Animation = Global.StartupCode$WebSharper_UI$Animation || {};
    AppendList = UI.AppendList = UI.AppendList || {};
    HashSetUtil = Collections.HashSetUtil = Collections.HashSetUtil || {};
    DynamicAttrNode = UI.DynamicAttrNode = UI.DynamicAttrNode || {};
    Char = WebSharper.Char = WebSharper.Char || {};
    DateUtil = WebSharper.DateUtil = WebSharper.DateUtil || {};
    Error = Global.Error;
    KeyNotFoundException = WebSharper.KeyNotFoundException = WebSharper.KeyNotFoundException || {};
    Scheduler = Concurrency.Scheduler = Concurrency.Scheduler || {};
    Easing = UI.Easing = UI.Easing || {};
    AsyncBody = Concurrency.AsyncBody = Concurrency.AsyncBody || {};
    SC$9 = Global.StartupCode$WebSharper_Main$Concurrency = Global.StartupCode$WebSharper_Main$Concurrency || {};
    CT = Concurrency.CT = Concurrency.CT || {};
    HashSet$1 = Abbrev.HashSet = Abbrev.HashSet || {};
    CancellationTokenSource = WebSharper.CancellationTokenSource = WebSharper.CancellationTokenSource || {};
    DomNodes = Docs$1.DomNodes = Docs$1.DomNodes || {};
    OperationCanceledException = WebSharper.OperationCanceledException = WebSharper.OperationCanceledException || {};
    Lazy = WebSharper.Lazy = WebSharper.Lazy || {};
    SC$10 = Global.StartupCode$WebSharper_UI$AppendList = Global.StartupCode$WebSharper_UI$AppendList || {};
    LazyExtensionsProxy = WebSharper.LazyExtensionsProxy = WebSharper.LazyExtensionsProxy || {};
    LazyRecord = LazyExtensionsProxy.LazyRecord = LazyExtensionsProxy.LazyRecord || {};
    Runtime = WebSharper && WebSharper.Runtime;
    console = Global.console;
    Date = Global.Date;
    ContactManager.Main = function () {
        var formInit, _;
        formInit = Contact.New("", "", "", "", "");
        _ = Form.Render(Runtime.Curried(ContactManager.RenderContact, 6), Form.Run(function (p) {
            var m, index;
            m = ContactManager.editIndex().Get();
            if (m == null)
                ContactManager.contactList().Set(new T({
                    $: 1,
                    $0: p,
                    $1: ContactManager.contactList().Get()
                }));
            else {
                index = m.$0;
                ContactManager.contactList().Set(List.mapi(function ($1, $2) {
                    return $1 === index ? p : $2;
                }, ContactManager.contactList().Get()));
                ContactManager.editIndex().Set(null);
            }
            Global.alert("Your Contact: " + p.firstName + " " + p.lastName + "!");
            formInit = Contact.New("", "", "", "", "");
        }, ContactManager.ContactForm(formInit)));
        Templates.LoadLocalTemplates("");
        Doc.RunById("main", _);
    };
    ContactManager.editIndex = function () {
        SC$1.$cctor();
        return SC$1.editIndex;
    };
    ContactManager.contactList = function () {
        SC$1.$cctor();
        return SC$1.contactList;
    };
    ContactManager.RenderContact = function (firstName, lastName, email, phone, address, submit) {
        return Doc.Element("section", [AttrProxy.Create("class", "section")], [Doc.Element("h2", [AttrProxy.Create("class", "subtitle")], [Doc.TextNode("Student Contact")]), Doc.Element("p", [AttrProxy.Create("class", "subhead")], [Doc.TextNode("A simple Website that stores, Updates and Deletes Student Contact Information")]), Doc.Element("div", [AttrProxy.Create("class", "field")], [Doc.Element("label", [AttrProxy.Create("class", "label")], [Doc.TextNode("First name: "), Doc.Input([AttrProxy.Create("class", "input")], firstName)]), ContactManager.ShowErrorsFor(View$1.Through$1(submit.view, firstName))]), Doc.Element("div", [AttrProxy.Create("class", "field")], [Doc.Element("label", [AttrProxy.Create("class", "label")], [Doc.TextNode("Last name: "), Doc.Input([AttrProxy.Create("class", "input")], lastName)]), ContactManager.ShowErrorsFor(View$1.Through$1(submit.view, lastName))]), Doc.Element("div", [AttrProxy.Create("class", "field")], [Doc.Element("label", [AttrProxy.Create("class", "label")], [Doc.TextNode("Email: "), Doc.EmailInput([AttrProxy.Create("class", "input")], email)]), ContactManager.ShowErrorsFor(View$1.Through$1(submit.view, email))]), Doc.Element("div", [AttrProxy.Create("class", "field")], [Doc.Element("label", [AttrProxy.Create("class", "label")], [Doc.TextNode("Phone: "), Doc.Input([AttrProxy.Create("class", "input")], phone)]), ContactManager.ShowErrorsFor(View$1.Through$1(submit.view, phone))]), Doc.Element("div", [AttrProxy.Create("class", "field")], [Doc.Element("label", [AttrProxy.Create("class", "label")], [Doc.TextNode("Address: "), Doc.Input([AttrProxy.Create("class", "input")], address)]), ContactManager.ShowErrorsFor(View$1.Through$1(submit.view, address))]), Doc.Element("div", [], [Doc.Button("Submit", [AttrProxy.Create("class", "button")], function () {
            submit.Trigger();
        })]), Doc.Element("div", [], [Doc.Button("View Contacts", [AttrProxy.Create("class", "button")], function () {
            ContactManager.showModal();
        })]), Doc.EmbedView(View.Map(function (visible) {
            return visible ? Doc.Element("div", [AttrProxy.Create("class", "modal is-active")], [Doc.Element("div", [AttrProxy.Create("class", "modal-background")], [Doc.Button("", [], function () {
                ContactManager.hideModal();
            })]), Doc.Element("div", [AttrProxy.Create("class", "modal-content")], [Doc.Element("div", [AttrProxy.Create("class", "box")], [Doc.Element("h2", [AttrProxy.Create("class", "subtitle")], [Doc.TextNode("Contact List")]), Doc.BindView(function (a) {
                return a.$ == 0 ? Doc.Element("p", [], [Doc.TextNode("No Contacts available")]) : Doc.Element("ul", [AttrProxy.Create("class", "listing")], List.mapi(function (index, contact) {
                    return Doc.Element("li", [], [Doc.TextNode("* - " + Utils.toSafe(contact.firstName) + " " + Utils.toSafe(contact.lastName) + " (" + Utils.toSafe(contact.email) + ", " + Utils.toSafe(contact.phone) + ", " + Utils.toSafe(contact.address) + ")"), Doc.Button("Edit", [AttrProxy.Create("class", "button is-small")], function () {
                        ContactManager.editIndex().Set({
                            $: 1,
                            $0: index
                        });
                        firstName.Set(contact.firstName);
                        lastName.Set(contact.lastName);
                        email.Set(contact.email);
                        phone.Set(contact.phone);
                        address.Set(contact.address);
                    }), Doc.Button("Delete", [AttrProxy.Create("class", "button is-small")], function () {
                        ContactManager.contactList().Set(List.choose(Global.id, List.mapi(function ($1, $2) {
                            return $1 !== index ? {
                                $: 1,
                                $0: $2
                            } : null;
                        }, ContactManager.contactList().Get())));
                    })]);
                }, a));
            }, ContactManager.contactList().get_View())])]), Doc.Element("div", [AttrProxy.Create("class", "modal-close is-large")], [Doc.Button("", [], function () {
                ContactManager.hideModal();
            })])]) : Doc.get_Empty();
        }, ContactManager.modalVisible().get_View()))]);
    };
    ContactManager.ContactForm = function (init) {
        return Form.WithSubmit(Pervasives$1.op_LessMultiplyGreater(Pervasives$1.op_LessMultiplyGreater(Pervasives$1.op_LessMultiplyGreater(Pervasives$1.op_LessMultiplyGreater(Pervasives$1.op_LessMultiplyGreater(Form.Return(Runtime.Curried(Contact.New, 5)), Validation.IsNotEmpty("Please enter the first name.", Form.Yield(init.firstName))), Validation.IsNotEmpty("Please enter the last name.", Form.Yield(init.lastName))), Validation.IsMatch("^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w+([-.]\\w+)*$", "Invalid email address.", Form.Yield(init.email))), Validation.IsMatch("^[0-9]+$", "Invalid phone number format.", Form.Yield(init.phone))), Form.Yield(init.address)));
    };
    ContactManager.ShowErrorsFor = function (v) {
        return Doc.EmbedView(View.Map(function (a) {
            var errors;
            return a.$ == 1 ? (errors = a.$0, Doc.Concat(List.ofSeq(Seq.delay(function () {
                return Seq.map(function (error) {
                    return Doc.Element("b", [AttrProxy.Create("style", "color:red")], [Doc.TextNode(error.message)]);
                }, errors);
            })))) : Doc.get_Empty();
        }, v));
    };
    ContactManager.showModal = function () {
        ContactManager.modalVisible().Set(true);
    };
    ContactManager.modalVisible = function () {
        SC$1.$cctor();
        return SC$1.modalVisible;
    };
    ContactManager.hideModal = function () {
        ContactManager.modalVisible().Set(false);
    };
    Contact.New = function (firstName, lastName, email, phone, address) {
        return {
            firstName: firstName,
            lastName: lastName,
            email: email,
            phone: phone,
            address: address
        };
    };
    Operators.FailWith = function (msg) {
        throw new Error(msg);
    };
    Operators.KeyValue = function (kvp) {
        return [kvp.K, kvp.V];
    };
    Operators.range = function (min, max) {
        var count;
        count = 1 + max - min;
        return count <= 0 ? [] : Seq.init(count, function (x) {
            return x + min;
        });
    };
    Form.Run = function (f, p) {
        return Form.Map(function (x) {
            f(x);
            return x;
        }, p);
    };
    Form.Render = function (renderFunction, p) {
        var x;
        x = p.render(renderFunction);
        return Doc.Append(Doc.EmbedView(View.Map(function () {
            return Doc.get_Empty();
        }, p.view)), x);
    };
    Form.Map = function (f, p) {
        return Form.MapResult(function (a) {
            return Result.Map(f, a);
        }, p);
    };
    Form.Return = function (value) {
        return Form.New(Fresh.Id(), View.Const({
            $: 0,
            $0: value
        }), Global.id);
    };
    Form.Yield = function (init) {
        return Form.YieldVar(Var$1.Create$1(init));
    };
    Form.WithSubmit = function (p) {
        var submitter;
        submitter = new Submitter.New(p.view, {
            $: 1,
            $0: T.Empty
        });
        return Form.New(Fresh.Id(), submitter.view, function (r) {
            return (p.render(r))(submitter);
        });
    };
    Form.MapResult = function (f, p) {
        return Form.New(p.id, View.Map(f, p.view), p.render);
    };
    Form.Apply = function (pf, px) {
        var f, g;
        return Form.New(Fresh.Id(), View.Map2(Result.Apply, pf.view, px.view), (f = pf.render, (g = px.render, function (x) {
            return g(f(x));
        })));
    };
    Form.YieldVar = function (_var) {
        return Form.New(_var.get_Id(), View.Map(function (a) {
            return {
                $: 0,
                $0: a
            };
        }, _var.get_View()), function (r) {
            return r(_var);
        });
    };
    Obj = WebSharper.Obj = Runtime.Class({
        Equals: function (obj) {
            return this === obj;
        },
        GetHashCode: function () {
            return -1;
        }
    }, null, Obj);
    Obj.New = Runtime.Ctor(function () {
    }, Obj);
    Var = UI.Var = Runtime.Class({}, Obj, Var);
    Var.New = Runtime.Ctor(function () {
        Obj.New.call(this);
    }, Var);
    T = List.T = Runtime.Class({
        GetEnumerator: function () {
            return new T$1.New(this, null, function (e) {
                var m;
                m = e.s;
                return m.$ == 0 ? false : (e.c = m.$0, e.s = m.$1, true);
            }, void 0);
        }
    }, null, T);
    T.Empty = new T({
        $: 0
    });
    List.mapi = function (f, x) {
        var r, l, i, go, res, t;
        if (x.$ == 0)
            return x;
        else {
            res = new T({
                $: 1
            });
            r = res;
            l = x;
            i = 0;
            go = true;
            while (go) {
                r.$0 = f(i, l.$0);
                l = l.$1;
                if (l.$ == 0)
                    go = false;
                else {
                    r = (t = new T({
                        $: 1
                    }), r.$1 = t, t);
                    i = i + 1;
                }
            }
            r.$1 = T.Empty;
            return res;
        }
    };
    List.choose = function (f, l) {
        return List.ofSeq(Seq.choose(f, l));
    };
    List.filter = function (p, x) {
        var res, r, l, go, t, v;
        if (x.$ == 0)
            return x;
        else {
            res = T.Empty;
            r = res;
            l = x;
            go = true;
            while (go) {
                if (p(l.$0)) {
                    res.$ == 0 ? (res = new T({
                        $: 1
                    }), r = res) : r = (t = new T({
                        $: 1
                    }), r.$1 = t, t);
                    v = l.$0;
                    r.$ = 1;
                    r.$0 = v;
                }
                l = l.$1;
                if (l.$ == 0)
                    go = false;
            }
            if (!(res.$ == 0))
                r.$1 = T.Empty;
            return res;
        }
    };
    List.ofSeq = function (s) {
        var e, $1, go, r, res, t;
        if (s instanceof T)
            return s;
        else
            if (s instanceof Global.Array)
                return List.ofArray(s);
            else {
                e = Enumerator.Get(s);
                try {
                    go = e.MoveNext();
                    if (!go)
                        $1 = T.Empty;
                    else {
                        res = new T({
                            $: 1
                        });
                        r = res;
                        while (go) {
                            r.$0 = e.Current();
                            if (e.MoveNext())
                                r = (t = new T({
                                    $: 1
                                }), r.$1 = t, t);
                            else
                                go = false;
                        }
                        r.$1 = T.Empty;
                        $1 = res;
                    }
                    return $1;
                }
                finally {
                    if (typeof e == "object" && "Dispose" in e)
                        e.Dispose();
                }
            }
    };
    List.ofArray = function (arr) {
        var r, i, $1;
        r = T.Empty;
        for (i = Arrays.length(arr) - 1, $1 = 0; i >= $1; i--)r = new T({
            $: 1,
            $0: Arrays.get(arr, i),
            $1: r
        });
        return r;
    };
    List.append = function (x, y) {
        var r, l, go, res, t;
        if (x.$ == 0)
            return y;
        else
            if (y.$ == 0)
                return x;
            else {
                res = new T({
                    $: 1
                });
                r = res;
                l = x;
                go = true;
                while (go) {
                    r.$0 = l.$0;
                    l = l.$1;
                    if (l.$ == 0)
                        go = false;
                    else
                        r = (t = new T({
                            $: 1
                        }), r.$1 = t, t);
                }
                r.$1 = y;
                return res;
            }
    };
    List.head = function (l) {
        return l.$ == 1 ? l.$0 : List.listEmpty();
    };
    List.tail = function (l) {
        return l.$ == 1 ? l.$1 : List.listEmpty();
    };
    List.listEmpty = function () {
        return Operators.FailWith("The input list was empty.");
    };
    JS.GetFieldValues = function (o) {
        var r, k;
        r = [];
        for (var k$1 in o) r.push(o[k$1]);
        return r;
    };
    SC$1.$cctor = function () {
        SC$1.$cctor = Global.ignore;
        SC$1.contactList = Var$1.Create$1(T.Empty);
        SC$1.editIndex = Var$1.Create$1(null);
        SC$1.modalVisible = Var$1.Create$1(false);
    };
    Pervasives.NewFromSeq = function (fields) {
        var r, e, f;
        r = {};
        e = Enumerator.Get(fields);
        try {
            while (e.MoveNext()) {
                f = e.Current();
                r[f[0]] = f[1];
            }
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
        return r;
    };
    Form.New = function (id, view, render) {
        return {
            id: id,
            view: view,
            render: render
        };
    };
    Doc = UI.Doc = Runtime.Class({}, Obj, Doc);
    Doc.Append = function (a, b) {
        return Doc.Mk({
            $: 0,
            $0: a.docNode,
            $1: b.docNode
        }, View.Map2Unit(a.updates, b.updates));
    };
    Doc.get_Empty = function () {
        return Doc.Mk(null, View.Const());
    };
    Doc.RunById = function (id, tr) {
        var m;
        m = self.document.getElementById(id);
        if (Unchecked.Equals(m, null))
            Operators.FailWith("invalid id: " + id);
        else
            Doc.Run(m, tr);
    };
    Doc.Mk = function (node, updates) {
        return new Doc.New(node, updates);
    };
    Doc.EmbedView = function (view) {
        var node;
        node = Docs.CreateEmbedNode();
        return Doc.Mk({
            $: 2,
            $0: node
        }, View.Map(Global.ignore, View.Bind(function (doc) {
            Docs.UpdateEmbedNode(node, doc.docNode);
            return doc.updates;
        }, view)));
    };
    Doc.TextNode = function (v) {
        return Doc.Mk({
            $: 5,
            $0: self.document.createTextNode(v)
        }, View.Const());
    };
    Doc.Input = function (attr$1, _var) {
        return Doc.InputInternal("input", function () {
            return Seq.append(attr$1, [AttrModule.Value(_var)]);
        });
    };
    Doc.Concat = function (xs) {
        var x;
        x = Array.ofSeqNonCopying(xs);
        return Array.TreeReduce(Doc.get_Empty(), Doc.Append, x);
    };
    Doc.EmailInput = function (attr$1, _var) {
        return Doc.InputInternal("input", function () {
            return Seq.append(attr$1, [AttrModule.Value(_var), AttrProxy.Create("type", "email")]);
        });
    };
    Doc.Button = function (caption, attrs, action) {
        var attrs$1;
        attrs$1 = AttrProxy.Concat(attrs);
        return Elt.New(Doc.Clickable("button", action), attrs$1, Doc.TextNode(caption));
    };
    Doc.Run = function (parent, doc) {
        Docs.LinkElement(parent, doc.docNode);
        Doc.RunInPlace(false, parent, doc);
    };
    Doc.Element = function (name, attr$1, children) {
        var a, a$1;
        a = AttrProxy.Concat(attr$1);
        a$1 = Doc.Concat(children);
        return Elt.New(self.document.createElement(name), a, a$1);
    };
    Doc.InputInternal = function (elemTy, attr$1) {
        var el;
        el = self.document.createElement(elemTy);
        return Elt.New(el, AttrProxy.Concat(attr$1(el)), Doc.get_Empty());
    };
    Doc.Clickable = function (elem, action) {
        var el;
        el = self.document.createElement(elem);
        el.addEventListener("click", function (ev) {
            ev.preventDefault();
            return action();
        }, false);
        return el;
    };
    Doc.BindView = function (f, view) {
        return Doc.EmbedView(View.Map(f, view));
    };
    Doc.RunInPlace = function (childrenOnly, parent, doc) {
        var st;
        st = Docs.CreateRunState(parent, doc.docNode);
        View.Sink(An.get_UseAnimations() || Settings.BatchUpdatesEnabled() ? Mailbox.StartProcessor(Docs.PerformAnimatedUpdate(childrenOnly, st, doc.docNode)) : function () {
            Docs.PerformSyncUpdate(childrenOnly, st, doc.docNode);
        }, doc.updates);
    };
    Doc.New = Runtime.Ctor(function (docNode, updates) {
        Obj.New.call(this);
        this.docNode = docNode;
        this.updates = updates;
    }, Doc);
    View.Map = function (fn, a) {
        return View.CreateLazy(function () {
            return Snap.Map(fn, a());
        });
    };
    View.Map2Unit = function (a, a$1) {
        return View.CreateLazy(function () {
            return Snap.Map2Unit(a(), a$1());
        });
    };
    View.CreateLazy = function (observe) {
        var lv;
        lv = {
            c: null,
            o: observe
        };
        return function () {
            var c, $1;
            c = lv.c;
            return c === null ? (c = lv.o(), lv.c = c, ($1 = c.s, $1 != null && $1.$ == 0) ? lv.o = null : Snap.WhenObsoleteRun(c, function () {
                lv.c = null;
            }), c) : c;
        };
    };
    View.Const = function (x) {
        var o;
        o = Snap.New({
            $: 0,
            $0: x
        });
        return function () {
            return o;
        };
    };
    View.Bind = function (fn, view) {
        return View.Join(View.Map(fn, view));
    };
    View.Map2 = function (fn, a, a$1) {
        return View.CreateLazy(function () {
            return Snap.Map2(fn, a(), a$1());
        });
    };
    View.Join = function (a) {
        return View.CreateLazy(function () {
            return Snap.Join(a());
        });
    };
    View.Sink = function (act, a) {
        function loop() {
            Snap.WhenRun(a(), act, function () {
                Concurrency.scheduler().Fork(loop);
            });
        }
        Concurrency.scheduler().Fork(loop);
    };
    View.SnapshotOn = function (def, a, a$1) {
        var sInit;
        sInit = Snap.New({
            $: 2,
            $0: def,
            $1: []
        });
        return View.CreateLazy(function () {
            return sInit.s == null ? Snap.SnapshotOn(a(), a$1()) : (Snap.WhenObsolete(a(), sInit), sInit);
        });
    };
    attr = HtmlModule.attr = Runtime.Class({}, Obj, attr);
    View$1.Through$1 = function (input, v) {
        return View.Map(function (x) {
            return x.$ == 1 ? {
                $: 1,
                $0: List.filter(function (m) {
                    return m.id == v.get_Id();
                }, x.$0)
            } : x;
        }, input);
    };
    Submitter = UI.Submitter = Runtime.Class({
        Trigger: function () {
            this["var"].Set(null);
        }
    }, Obj, Submitter);
    Submitter.New = Runtime.Ctor(function (input, init) {
        Obj.New.call(this);
        this.input = input;
        this["var"] = Var$1.Create();
        this.view = View.SnapshotOn(init, this["var"].get_View(), this.input);
    }, Submitter);
    Utils.toSafe = function (s) {
        return s == null ? "" : s;
    };
    Templates.LoadLocalTemplates = function (baseName) {
        !Templates.LocalTemplatesLoaded() ? (Templates.set_LocalTemplatesLoaded(true), Templates.LoadNestedTemplates(self.document.body, "")) : void 0;
        Templates.LoadedTemplates().set_Item(baseName, Templates.LoadedTemplateFile(""));
    };
    Templates.LocalTemplatesLoaded = function () {
        SC$2.$cctor();
        return SC$2.LocalTemplatesLoaded;
    };
    Templates.set_LocalTemplatesLoaded = function ($1) {
        SC$2.$cctor();
        SC$2.LocalTemplatesLoaded = $1;
    };
    Templates.LoadNestedTemplates = function (root, baseName) {
        var loadedTpls, rawTpls, wsTemplates, i, $1, node, name, wsChildrenTemplates, i$1, $2, node$1, name$1, html5TemplateBasedTemplates, i$2, $3, node$2, html5TemplateBasedTemplates$1, i$3, $4, node$3, instantiated;
        function prepareTemplate(name$2) {
            var m, o;
            if (!loadedTpls.ContainsKey(name$2)) {
                m = (o = null, [rawTpls.TryGetValue(name$2, {
                    get: function () {
                        return o;
                    },
                    set: function (v) {
                        o = v;
                    }
                }), o]);
                if (m[0]) {
                    instantiated.SAdd(name$2);
                    rawTpls.RemoveKey(name$2);
                    Templates.PrepareTemplateStrict(baseName, {
                        $: 1,
                        $0: name$2
                    }, m[1], {
                        $: 1,
                        $0: prepareTemplate
                    });
                }
                else
                    console.warn(instantiated.Contains(name$2) ? "Encountered loop when instantiating " + name$2 : "Local template does not exist: " + name$2);
            }
        }
        loadedTpls = Templates.LoadedTemplateFile(baseName);
        rawTpls = new Dictionary.New$5();
        wsTemplates = root.querySelectorAll("[ws-template]");
        for (i = 0, $1 = wsTemplates.length - 1; i <= $1; i++) {
            node = wsTemplates[i];
            name = node.getAttribute("ws-template").toLowerCase();
            node.removeAttribute("ws-template");
            rawTpls.set_Item(name, Templates.FakeRootSingle(node));
        }
        wsChildrenTemplates = root.querySelectorAll("[ws-children-template]");
        for (i$1 = 0, $2 = wsChildrenTemplates.length - 1; i$1 <= $2; i$1++) {
            node$1 = wsChildrenTemplates[i$1];
            name$1 = node$1.getAttribute("ws-children-template").toLowerCase();
            node$1.removeAttribute("ws-children-template");
            rawTpls.set_Item(name$1, Templates.FakeRoot(node$1));
        }
        html5TemplateBasedTemplates = root.querySelectorAll("template[id]");
        for (i$2 = 0, $3 = html5TemplateBasedTemplates.length - 1; i$2 <= $3; i$2++) {
            node$2 = html5TemplateBasedTemplates[i$2];
            rawTpls.set_Item(node$2.getAttribute("id").toLowerCase(), Templates.FakeRootFromHTMLTemplate(node$2));
        }
        html5TemplateBasedTemplates$1 = root.querySelectorAll("template[name]");
        for (i$3 = 0, $4 = html5TemplateBasedTemplates$1.length - 1; i$3 <= $4; i$3++) {
            node$3 = html5TemplateBasedTemplates$1[i$3];
            rawTpls.set_Item(node$3.getAttribute("name").toLowerCase(), Templates.FakeRootFromHTMLTemplate(node$3));
        }
        instantiated = new HashSet.New$3();
        while (rawTpls.count > 0)
            prepareTemplate(Seq.head(rawTpls.Keys()));
    };
    Templates.LoadedTemplates = function () {
        SC$2.$cctor();
        return SC$2.LoadedTemplates;
    };
    Templates.LoadedTemplateFile = function (name) {
        var m, o, d;
        m = (o = null, [Templates.LoadedTemplates().TryGetValue(name, {
            get: function () {
                return o;
            },
            set: function (v) {
                o = v;
            }
        }), o]);
        return m[0] ? m[1] : (d = new Dictionary.New$5(), (Templates.LoadedTemplates().set_Item(name, d), d));
    };
    Templates.FakeRootSingle = function (el) {
        var m, m$1, n, fakeroot;
        el.removeAttribute("ws-template");
        m = el.getAttribute("ws-replace");
        if (m == null)
            ;
        else {
            el.removeAttribute("ws-replace");
            m$1 = el.parentNode;
            Unchecked.Equals(m$1, null) ? void 0 : (n = self.document.createElement(el.tagName), n.setAttribute("ws-replace", m), m$1.replaceChild(n, el));
        }
        fakeroot = self.document.createElement("div");
        fakeroot.appendChild(el);
        return fakeroot;
    };
    Templates.FakeRoot = function (parent) {
        var fakeroot;
        fakeroot = self.document.createElement("div");
        while (parent.hasChildNodes())
            fakeroot.appendChild(parent.firstChild);
        return fakeroot;
    };
    Templates.FakeRootFromHTMLTemplate = function (parent) {
        var fakeroot, content, i, $1;
        fakeroot = self.document.createElement("div");
        content = parent.content;
        for (i = 0, $1 = content.childNodes.length - 1; i <= $1; i++)fakeroot.appendChild(content.childNodes[i].cloneNode(true));
        return fakeroot;
    };
    Templates.PrepareTemplateStrict = function (baseName, name, fakeroot, prepareLocalTemplate) {
        var processedHTML5Templates, name$1;
        function recF(recI, $1) {
            var next, m, $2, x, f, name$2, p, instName, instBaseName, d, t, instance, usedHoles, mappings, attrs, i, $3, name$3, m$1, i$1, $4, n, singleTextFill, i$2, $5, n$1;
            function g(v) {
            }
            while (true)
                switch (recI) {
                    case 0:
                        if ($1 !== null) {
                            next = $1.nextSibling;
                            if (Unchecked.Equals($1.nodeType, Node.TEXT_NODE))
                                Prepare.convertTextNode($1);
                            else
                                if (Unchecked.Equals($1.nodeType, Node.ELEMENT_NODE))
                                    convertElement($1);
                            $1 = next;
                        }
                        else
                            return null;
                        break;
                    case 1:
                        name$2 = Slice.string($1.nodeName, {
                            $: 1,
                            $0: 3
                        }, null).toLowerCase();
                        p = (m = name$2.indexOf("."), m === -1 ? [baseName, name$2] : [Slice.string(name$2, null, {
                            $: 1,
                            $0: m - 1
                        }), Slice.string(name$2, {
                            $: 1,
                            $0: m + 1
                        }, null)]);
                        instName = p[1];
                        instBaseName = p[0];
                        if (instBaseName != "" && !Templates.LoadedTemplates().ContainsKey(instBaseName))
                            return Prepare.failNotLoaded(instName);
                        else {
                            if (instBaseName == "" && prepareLocalTemplate != null)
                                prepareLocalTemplate.$0(instName);
                            d = Templates.LoadedTemplates().Item(instBaseName);
                            if (!d.ContainsKey(instName))
                                return Prepare.failNotLoaded(instName);
                            else {
                                t = d.Item(instName);
                                instance = t.cloneNode(true);
                                usedHoles = new HashSet.New$3();
                                mappings = new Dictionary.New$5();
                                attrs = $1.attributes;
                                for (i = 0, $3 = attrs.length - 1; i <= $3; i++) {
                                    name$3 = attrs.item(i).name.toLowerCase();
                                    mappings.set_Item(name$3, (m$1 = attrs.item(i).nodeValue, m$1 == "" ? name$3 : m$1.toLowerCase()));
                                    if (!usedHoles.SAdd(name$3))
                                        console.warn("Hole mapped twice", name$3);
                                }
                                for (i$1 = 0, $4 = $1.childNodes.length - 1; i$1 <= $4; i$1++) {
                                    n = $1.childNodes[i$1];
                                    if (Unchecked.Equals(n.nodeType, Node.ELEMENT_NODE))
                                        !usedHoles.SAdd(n.nodeName.toLowerCase()) ? console.warn("Hole filled twice", instName) : void 0;
                                }
                                singleTextFill = $1.childNodes.length === 1 && Unchecked.Equals($1.firstChild.nodeType, Node.TEXT_NODE);
                                if (singleTextFill) {
                                    x = Prepare.fillTextHole(instance, $1.firstChild.textContent, instName);
                                    ((function (a) {
                                        return function (o) {
                                            if (o != null)
                                                a(o.$0);
                                        };
                                    }((f = function (usedHoles$1) {
                                        return function (a) {
                                            return usedHoles$1.SAdd(a);
                                        };
                                    }(usedHoles), function (x$1) {
                                            return g(f(x$1));
                                        })))(x));
                                }
                                Prepare.removeHolesExcept(instance, usedHoles);
                                if (!singleTextFill) {
                                    for (i$2 = 0, $5 = $1.childNodes.length - 1; i$2 <= $5; i$2++) {
                                        n$1 = $1.childNodes[i$2];
                                        if (Unchecked.Equals(n$1.nodeType, Node.ELEMENT_NODE))
                                            n$1.hasAttributes() ? Prepare.fillInstanceAttrs(instance, n$1) : fillDocHole(instance, n$1);
                                    }
                                }
                                Prepare.mapHoles(instance, mappings);
                                Prepare.fill(instance, $1.parentNode, $1);
                                $1.parentNode.removeChild($1);
                                return;
                            }
                        }
                        break;
                }
        }
        function fillDocHole(instance, fillWith) {
            var m, m$1, name$2, m$2;
            function fillHole(p, n) {
                var parsed;
                if (name$2 == "title" && fillWith.hasChildNodes()) {
                    parsed = DomUtility.ParseHTMLIntoFakeRoot(fillWith.textContent);
                    fillWith.removeChild(fillWith.firstChild);
                    while (parsed.hasChildNodes())
                        fillWith.appendChild(parsed.firstChild);
                }
                else
                    null;
                convertElement(fillWith);
                return Prepare.fill(fillWith, p, n);
            }
            name$2 = fillWith.nodeName.toLowerCase();
            Templates.foreachNotPreserved(instance, "[ws-attr-holes]", function (e) {
                var holeAttrs, i, $1, attrName, _this;
                holeAttrs = Strings.SplitChars(e.getAttribute("ws-attr-holes"), [" "], 1);
                for (i = 0, $1 = holeAttrs.length - 1; i <= $1; i++) {
                    attrName = Arrays.get(holeAttrs, i);
                    e.setAttribute(attrName, (_this = new Global.RegExp("\\${" + name$2 + "}", "ig"), e.getAttribute(attrName).replace(_this, fillWith.textContent)));
                }
            });
            m$2 = instance.querySelector("[ws-hole=" + name$2 + "]");
            if (Unchecked.Equals(m$2, null)) {
                m = instance.querySelector("[ws-replace=" + name$2 + "]");
                return Unchecked.Equals(m, null) ? (m$1 = instance.querySelector("slot[name=" + name$2 + "]"), instance.tagName.toLowerCase() == "template" ? (fillHole(m$1.parentNode, m$1), void m$1.parentNode.removeChild(m$1)) : null) : (fillHole(m.parentNode, m), void m.parentNode.removeChild(m));
            }
            else {
                while (m$2.hasChildNodes())
                    m$2.removeChild(m$2.lastChild);
                m$2.removeAttribute("ws-hole");
                return fillHole(m$2, null);
            }
        }
        function convertElement(el) {
            if (!el.hasAttribute("ws-preserve"))
                if (Strings.StartsWith(el.nodeName.toLowerCase(), "ws-"))
                    convertInstantiation(el);
                else {
                    Prepare.convertAttrs(el);
                    convertNodeAndSiblings(el.firstChild);
                }
        }
        function convertNodeAndSiblings(n) {
            return recF(0, n);
        }
        function convertInstantiation(el) {
            return recF(1, el);
        }
        function convertNestedTemplates(el) {
            var m, m$1, idTemplates, i, $1, n, nameTemplates, i$1, $2, n$1, name$2, name$3;
            while (true) {
                m = el.querySelector("[ws-template]");
                if (Unchecked.Equals(m, null)) {
                    m$1 = el.querySelector("[ws-children-template]");
                    if (Unchecked.Equals(m$1, null)) {
                        idTemplates = el.querySelectorAll("template[id]");
                        for (i = 1, $1 = idTemplates.length - 1; i <= $1; i++) {
                            n = idTemplates[i];
                            if (processedHTML5Templates.Contains(n))
                                ;
                            else {
                                Templates.PrepareTemplateStrict(baseName, {
                                    $: 1,
                                    $0: n.getAttribute("id")
                                }, n, null);
                                processedHTML5Templates.SAdd(n);
                            }
                        }
                        nameTemplates = el.querySelectorAll("template[name]");
                        for (i$1 = 1, $2 = nameTemplates.length - 1; i$1 <= $2; i$1++) {
                            n$1 = nameTemplates[i$1];
                            if (processedHTML5Templates.Contains(n$1))
                                ;
                            else {
                                Templates.PrepareTemplateStrict(baseName, {
                                    $: 1,
                                    $0: n$1.getAttribute("name")
                                }, n$1, null);
                                processedHTML5Templates.SAdd(n$1);
                            }
                        }
                        return null;
                    }
                    else {
                        name$2 = m$1.getAttribute("ws-children-template");
                        m$1.removeAttribute("ws-children-template");
                        Templates.PrepareTemplateStrict(baseName, {
                            $: 1,
                            $0: name$2
                        }, m$1, null);
                        el = el;
                    }
                }
                else {
                    name$3 = m.getAttribute("ws-template");
                    (Templates.PrepareSingleTemplate(baseName, {
                        $: 1,
                        $0: name$3
                    }, m))(null);
                    el = el;
                }
            }
        }
        processedHTML5Templates = new HashSet.New$3();
        name$1 = (name == null ? "" : name.$0).toLowerCase();
        Templates.LoadedTemplateFile(baseName).set_Item(name$1, fakeroot);
        if (fakeroot.hasChildNodes()) {
            convertNestedTemplates(fakeroot);
            convertNodeAndSiblings(fakeroot.firstChild);
        }
    };
    Templates.foreachNotPreserved = function (root, selector, f) {
        DomUtility.IterSelector(root, selector, function (p) {
            if (p.closest("[ws-preserve]") == null)
                f(p);
        });
    };
    Templates.PrepareSingleTemplate = function (baseName, name, el) {
        var root;
        root = Templates.FakeRootSingle(el);
        return function (p) {
            Templates.PrepareTemplateStrict(baseName, name, root, p);
        };
    };
    Templates.TextHoleRE = function () {
        SC$2.$cctor();
        return SC$2.TextHoleRE;
    };
    Pervasives$1.op_LessMultiplyGreater = function (pf, px) {
        return Form.Apply(pf, px);
    };
    Validation.IsNotEmpty = function (msg, p) {
        return Validation.Is(function (x) {
            return x != "";
        }, msg, p);
    };
    Validation.IsMatch = function (regexp, msg, p) {
        var o;
        return Validation.Is((o = new Global.RegExp(regexp), function (a) {
            return o.test(a);
        }), msg, p);
    };
    Validation.Is = function (pred, msg, p) {
        return Form.MapResult(function (res) {
            return res.$ == 1 ? res : pred(res.$0) ? res : {
                $: 1,
                $0: List.ofArray([new ErrorMessage.New(p.id, msg)])
            };
        }, p);
    };
    Result.Map = function (f, r) {
        return r.$ == 1 ? {
            $: 1,
            $0: r.$0
        } : {
            $: 0,
            $0: f(r.$0)
        };
    };
    Result.Apply = function (rf, rx) {
        return rf.$ == 0 ? rx.$ == 0 ? {
            $: 0,
            $0: rf.$0(rx.$0)
        } : {
            $: 1,
            $0: rx.$0
        } : rx.$ == 0 ? {
            $: 1,
            $0: rf.$0
        } : {
            $: 1,
            $0: List.append(rf.$0, rx.$0)
        };
    };
    Var$1 = UI.Var$1 = Runtime.Class({}, Obj, Var$1);
    Var$1.Create$1 = function (v) {
        return new ConcreteVar.New(false, Snap.New({
            $: 2,
            $0: v,
            $1: []
        }), v);
    };
    Var$1.Create = function () {
        return new ConcreteVar.New(false, Snap.New({
            $: 2,
            $0: null,
            $1: []
        }), null);
    };
    View = UI.View = Runtime.Class({}, null, View);
    Snap.Map = function (fn, sn) {
        var m, res;
        m = sn.s;
        return m != null && m.$ == 0 ? Snap.New({
            $: 0,
            $0: fn(m.$0)
        }) : (res = Snap.New({
            $: 3,
            $0: [],
            $1: []
        }), (Snap.When(sn, function (a) {
            Snap.MarkDone(res, sn, fn(a));
        }, res), res));
    };
    Snap.Map2Unit = function (sn1, sn2) {
        var $1, $2, res;
        function cont() {
            var m, $3, $4;
            if (!(m = res.s, m != null && m.$ == 0 || m != null && m.$ == 2)) {
                $3 = Snap.ValueAndForever(sn1);
                $4 = Snap.ValueAndForever(sn2);
                if ($3 != null && $3.$ == 1)
                    $4 != null && $4.$ == 1 ? $3.$0[1] && $4.$0[1] ? Snap.MarkForever(res, null) : Snap.MarkReady(res, null) : void 0;
            }
        }
        $1 = sn1.s;
        $2 = sn2.s;
        return $1 != null && $1.$ == 0 ? $2 != null && $2.$ == 0 ? Snap.New({
            $: 0,
            $0: null
        }) : sn2 : $2 != null && $2.$ == 0 ? sn1 : (res = Snap.New({
            $: 3,
            $0: [],
            $1: []
        }), (Snap.When(sn1, cont, res), Snap.When(sn2, cont, res), res));
    };
    Snap.WhenObsoleteRun = function (snap, obs) {
        var m;
        m = snap.s;
        if (m == null)
            obs();
        else
            m != null && m.$ == 2 ? m.$1.push(obs) : m != null && m.$ == 3 ? m.$1.push(obs) : void 0;
    };
    Snap.When = function (snap, avail, obs) {
        var m;
        m = snap.s;
        if (m == null)
            Snap.Obsolete(obs);
        else
            m != null && m.$ == 2 ? (Snap.EnqueueSafe(m.$1, obs), avail(m.$0)) : m != null && m.$ == 3 ? (m.$0.push(avail), Snap.EnqueueSafe(m.$1, obs)) : avail(m.$0);
    };
    Snap.MarkDone = function (res, sn, v) {
        var $1;
        if ($1 = sn.s, $1 != null && $1.$ == 0)
            Snap.MarkForever(res, v);
        else
            Snap.MarkReady(res, v);
    };
    Snap.ValueAndForever = function (snap) {
        var m;
        m = snap.s;
        return m != null && m.$ == 0 ? {
            $: 1,
            $0: [m.$0, true]
        } : m != null && m.$ == 2 ? {
            $: 1,
            $0: [m.$0, false]
        } : null;
    };
    Snap.MarkForever = function (sn, v) {
        var m, qa, i, $1;
        m = sn.s;
        if (m != null && m.$ == 3) {
            sn.s = {
                $: 0,
                $0: v
            };
            qa = m.$0;
            for (i = 0, $1 = Arrays.length(qa) - 1; i <= $1; i++)(Arrays.get(qa, i))(v);
        }
        else
            void 0;
    };
    Snap.MarkReady = function (sn, v) {
        var m, qa, i, $1;
        m = sn.s;
        if (m != null && m.$ == 3) {
            sn.s = {
                $: 2,
                $0: v,
                $1: m.$1
            };
            qa = m.$0;
            for (i = 0, $1 = Arrays.length(qa) - 1; i <= $1; i++)(Arrays.get(qa, i))(v);
        }
        else
            void 0;
    };
    Snap.EnqueueSafe = function (q, x) {
        var qcopy, i, $1, o;
        q.push(x);
        if (q.length % 20 === 0) {
            qcopy = q.slice(0);
            Queue.Clear(q);
            for (i = 0, $1 = Arrays.length(qcopy) - 1; i <= $1; i++) {
                o = Arrays.get(qcopy, i);
                if (typeof o == "object")
                    (function (sn) {
                        if (sn.s)
                            q.push(sn);
                    }(o));
                else
                    (function (f) {
                        q.push(f);
                    }(o));
            }
        }
        else
            void 0;
    };
    Snap.Map2 = function (fn, sn1, sn2) {
        var $1, $2, res;
        function cont(a) {
            var m, $3, $4;
            if (!(m = res.s, m != null && m.$ == 0 || m != null && m.$ == 2)) {
                $3 = Snap.ValueAndForever(sn1);
                $4 = Snap.ValueAndForever(sn2);
                if ($3 != null && $3.$ == 1)
                    $4 != null && $4.$ == 1 ? $3.$0[1] && $4.$0[1] ? Snap.MarkForever(res, fn($3.$0[0], $4.$0[0])) : Snap.MarkReady(res, fn($3.$0[0], $4.$0[0])) : void 0;
            }
        }
        $1 = sn1.s;
        $2 = sn2.s;
        return $1 != null && $1.$ == 0 ? $2 != null && $2.$ == 0 ? Snap.New({
            $: 0,
            $0: fn($1.$0, $2.$0)
        }) : Snap.Map2Opt1(fn, $1.$0, sn2) : $2 != null && $2.$ == 0 ? Snap.Map2Opt2(fn, $2.$0, sn1) : (res = Snap.New({
            $: 3,
            $0: [],
            $1: []
        }), (Snap.When(sn1, cont, res), Snap.When(sn2, cont, res), res));
    };
    Snap.Join = function (snap) {
        var res;
        res = Snap.New({
            $: 3,
            $0: [],
            $1: []
        });
        Snap.When(snap, function (x) {
            var y;
            y = x();
            Snap.When(y, function (v) {
                var $1, $2;
                if (($1 = y.s, $1 != null && $1.$ == 0) && ($2 = snap.s, $2 != null && $2.$ == 0))
                    Snap.MarkForever(res, v);
                else
                    Snap.MarkReady(res, v);
            }, res);
        }, res);
        return res;
    };
    Snap.WhenRun = function (snap, avail, obs) {
        var m;
        m = snap.s;
        if (m == null)
            obs();
        else
            m != null && m.$ == 2 ? (m.$1.push(obs), avail(m.$0)) : m != null && m.$ == 3 ? (m.$0.push(avail), m.$1.push(obs)) : avail(m.$0);
    };
    Snap.Map2Opt1 = function (fn, x, sn2) {
        return Snap.Map(function (y) {
            return fn(x, y);
        }, sn2);
    };
    Snap.Map2Opt2 = function (fn, y, sn1) {
        return Snap.Map(function (x) {
            return fn(x, y);
        }, sn1);
    };
    Snap.SnapshotOn = function (sn1, sn2) {
        var res;
        function cont(a) {
            var m, $1, $2;
            if (!(m = res.s, m != null && m.$ == 0 || m != null && m.$ == 2)) {
                $1 = Snap.ValueAndForever(sn1);
                $2 = Snap.ValueAndForever(sn2);
                if ($1 != null && $1.$ == 1)
                    $2 != null && $2.$ == 1 ? $1.$0[1] || $2.$0[1] ? Snap.MarkForever(res, $2.$0[0]) : Snap.MarkReady(res, $2.$0[0]) : void 0;
            }
        }
        res = Snap.New({
            $: 3,
            $0: [],
            $1: []
        });
        Snap.When(sn1, cont, res);
        Snap.WhenReady(sn2, cont);
        return res;
    };
    Snap.WhenObsolete = function (snap, obs) {
        var m;
        m = snap.s;
        if (m == null)
            Snap.Obsolete(obs);
        else
            m != null && m.$ == 2 ? Snap.EnqueueSafe(m.$1, obs) : m != null && m.$ == 3 ? Snap.EnqueueSafe(m.$1, obs) : void 0;
    };
    Snap.Copy = function (sn) {
        var m, res, res$1;
        m = sn.s;
        return m == null ? sn : m != null && m.$ == 2 ? (res = Snap.New({
            $: 2,
            $0: m.$0,
            $1: []
        }), (Snap.WhenObsolete(sn, res), res)) : m != null && m.$ == 3 ? (res$1 = Snap.New({
            $: 3,
            $0: [],
            $1: []
        }), (Snap.When(sn, function (v) {
            Snap.MarkDone(res$1, sn, v);
        }, res$1), res$1)) : sn;
    };
    Snap.WhenReady = function (snap, avail) {
        var $1, m;
        m = snap.s;
        switch (m != null && m.$ == 2 ? ($1 = m.$0, 0) : m == null ? 1 : m != null && m.$ == 3 ? 2 : ($1 = m.$0, 0)) {
            case 0:
                avail($1);
                break;
            case 1:
                null;
                break;
            case 2:
                m.$0.push(avail);
                break;
        }
    };
    AttrProxy = UI.AttrProxy = Runtime.Class({}, null, AttrProxy);
    AttrProxy.Create = function (name, value) {
        return Attrs.Static(function (el) {
            el.setAttribute(name, value);
        });
    };
    AttrProxy.Concat = function (xs) {
        var x;
        x = Array.ofSeqNonCopying(xs);
        return Array.TreeReduce(Attrs.EmptyAttr(), AttrProxy.Append, x);
    };
    AttrProxy.Append = function (a, b) {
        return Attrs.AppendTree(a, b);
    };
    Seq.delay = function (f) {
        return {
            GetEnumerator: function () {
                return Enumerator.Get(f());
            }
        };
    };
    Seq.map = function (f, s) {
        return {
            GetEnumerator: function () {
                var en;
                en = Enumerator.Get(s);
                return new T$1.New(null, null, function (e) {
                    return en.MoveNext() && (e.c = f(en.Current()), true);
                }, function () {
                    en.Dispose();
                });
            }
        };
    };
    Seq.choose = function (f, s) {
        return Seq.collect(function (x) {
            var m;
            m = f(x);
            return m == null ? T.Empty : List.ofArray([m.$0]);
        }, s);
    };
    Seq.append = function (s1, s2) {
        return {
            GetEnumerator: function () {
                var e1, first;
                e1 = Enumerator.Get(s1);
                first = [true];
                return new T$1.New(e1, null, function (x) {
                    var x$1;
                    return x.s.MoveNext() ? (x.c = x.s.Current(), true) : (x$1 = x.s, !Unchecked.Equals(x$1, null) ? x$1.Dispose() : void 0, x.s = null, first[0] && (first[0] = false, x.s = Enumerator.Get(s2), x.s.MoveNext() ? (x.c = x.s.Current(), true) : (x.s.Dispose(), x.s = null, false)));
                }, function (x) {
                    var x$1;
                    x$1 = x.s;
                    if (!Unchecked.Equals(x$1, null))
                        x$1.Dispose();
                });
            }
        };
    };
    Seq.collect = function (f, s) {
        return Seq.concat(Seq.map(f, s));
    };
    Seq.head = function (s) {
        var e;
        e = Enumerator.Get(s);
        try {
            return e.MoveNext() ? e.Current() : Seq.insufficient();
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    };
    Seq.concat = function (ss) {
        return {
            GetEnumerator: function () {
                var outerE;
                function next(st) {
                    var m;
                    while (true) {
                        m = st.s;
                        if (Unchecked.Equals(m, null)) {
                            if (outerE.MoveNext()) {
                                st.s = Enumerator.Get(outerE.Current());
                                st = st;
                            }
                            else {
                                outerE.Dispose();
                                return false;
                            }
                        }
                        else
                            if (m.MoveNext()) {
                                st.c = m.Current();
                                return true;
                            }
                            else {
                                st.Dispose();
                                st.s = null;
                                st = st;
                            }
                    }
                }
                outerE = Enumerator.Get(ss);
                return new T$1.New(null, null, next, function (st) {
                    var x;
                    x = st.s;
                    if (!Unchecked.Equals(x, null))
                        x.Dispose();
                    if (!Unchecked.Equals(outerE, null))
                        outerE.Dispose();
                });
            }
        };
    };
    Seq.fold = function (f, x, s) {
        var r, e;
        r = x;
        e = Enumerator.Get(s);
        try {
            while (e.MoveNext())
                r = f(r, e.Current());
            return r;
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    };
    Seq.init = function (n, f) {
        return Seq.take(n, Seq.initInfinite(f));
    };
    Seq.iter = function (p, s) {
        var e;
        e = Enumerator.Get(s);
        try {
            while (e.MoveNext())
                p(e.Current());
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    };
    Seq.forall = function (p, s) {
        return !Seq.exists(function (x) {
            return !p(x);
        }, s);
    };
    Seq.take = function (n, s) {
        n < 0 ? Seq.nonNegative() : void 0;
        return {
            GetEnumerator: function () {
                var e;
                e = [Enumerator.Get(s)];
                return new T$1.New(0, null, function (o) {
                    var en;
                    o.s = o.s + 1;
                    return o.s > n ? false : (en = e[0], Unchecked.Equals(en, null) ? Seq.insufficient() : en.MoveNext() ? (o.c = en.Current(), o.s === n ? (en.Dispose(), e[0] = null) : void 0, true) : (en.Dispose(), e[0] = null, Seq.insufficient()));
                }, function () {
                    var x;
                    x = e[0];
                    if (!Unchecked.Equals(x, null))
                        x.Dispose();
                });
            }
        };
    };
    Seq.initInfinite = function (f) {
        return {
            GetEnumerator: function () {
                return new T$1.New(0, null, function (e) {
                    e.c = f(e.s);
                    e.s = e.s + 1;
                    return true;
                }, void 0);
            }
        };
    };
    Seq.max = function (s) {
        var e, m, x;
        e = Enumerator.Get(s);
        try {
            if (!e.MoveNext())
                Seq.seqEmpty();
            m = e.Current();
            while (e.MoveNext()) {
                x = e.Current();
                if (Unchecked.Compare(x, m) === 1)
                    m = x;
            }
            return m;
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    };
    Seq.exists = function (p, s) {
        var e, r;
        e = Enumerator.Get(s);
        try {
            r = false;
            while (!r && e.MoveNext())
                r = p(e.Current());
            return r;
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    };
    Seq.seqEmpty = function () {
        return Operators.FailWith("The input sequence was empty.");
    };
    ErrorMessage = Forms.ErrorMessage = Runtime.Class({}, Obj, ErrorMessage);
    ErrorMessage.New = Runtime.Ctor(function (id, message) {
        Obj.New.call(this);
        this.id = id;
        this.message = message;
    }, ErrorMessage);
    Unchecked.Equals = function (a, b) {
        var m, eqR, k, k$1;
        if (a === b)
            return true;
        else {
            m = typeof a;
            if (m == "object") {
                if (a === null || a === void 0 || b === null || b === void 0 || !Unchecked.Equals(typeof b, "object"))
                    return false;
                else
                    if ("Equals" in a)
                        return a.Equals(b);
                    else
                        if ("Equals" in b)
                            return false;
                        else
                            if (a instanceof Global.Array && b instanceof Global.Array)
                                return Unchecked.arrayEquals(a, b);
                            else
                                if (a instanceof Global.Date && b instanceof Global.Date)
                                    return Unchecked.dateEquals(a, b);
                                else {
                                    eqR = [true];
                                    for (var k$2 in a) if (function (k$3) {
                                        eqR[0] = !a.hasOwnProperty(k$3) || b.hasOwnProperty(k$3) && Unchecked.Equals(a[k$3], b[k$3]);
                                        return !eqR[0];
                                    }(k$2))
                                        break;
                                    if (eqR[0]) {
                                        for (var k$3 in b) if (function (k$4) {
                                            eqR[0] = !b.hasOwnProperty(k$4) || a.hasOwnProperty(k$4);
                                            return !eqR[0];
                                        }(k$3))
                                            break;
                                    }
                                    return eqR[0];
                                }
            }
            else
                return m == "function" && ("$Func" in a ? a.$Func === b.$Func && a.$Target === b.$Target : "$Invokes" in a && "$Invokes" in b && Unchecked.arrayEquals(a.$Invokes, b.$Invokes));
        }
    };
    Unchecked.arrayEquals = function (a, b) {
        var eq, i;
        if (Arrays.length(a) === Arrays.length(b)) {
            eq = true;
            i = 0;
            while (eq && i < Arrays.length(a)) {
                !Unchecked.Equals(Arrays.get(a, i), Arrays.get(b, i)) ? eq = false : void 0;
                i = i + 1;
            }
            return eq;
        }
        else
            return false;
    };
    Unchecked.dateEquals = function (a, b) {
        return a.getTime() === b.getTime();
    };
    Unchecked.Hash = function (o) {
        var m;
        m = typeof o;
        return m == "function" ? 0 : m == "boolean" ? o ? 1 : 0 : m == "number" ? o : m == "string" ? Unchecked.hashString(o) : m == "object" ? o == null ? 0 : o instanceof Global.Array ? Unchecked.hashArray(o) : Unchecked.hashObject(o) : 0;
    };
    Unchecked.hashString = function (s) {
        var hash, i, $1;
        if (s === null)
            return 0;
        else {
            hash = 5381;
            for (i = 0, $1 = s.length - 1; i <= $1; i++)hash = Unchecked.hashMix(hash, s[i].charCodeAt());
            return hash;
        }
    };
    Unchecked.hashArray = function (o) {
        var h, i, $1;
        h = -34948909;
        for (i = 0, $1 = Arrays.length(o) - 1; i <= $1; i++)h = Unchecked.hashMix(h, Unchecked.Hash(Arrays.get(o, i)));
        return h;
    };
    Unchecked.hashObject = function (o) {
        var h, k;
        if ("GetHashCode" in o)
            return o.GetHashCode();
        else {
            h = [0];
            for (var k$1 in o) if (function (key) {
                h[0] = Unchecked.hashMix(Unchecked.hashMix(h[0], Unchecked.hashString(key)), Unchecked.Hash(o[key]));
                return false;
            }(k$1))
                break;
            return h[0];
        }
    };
    Unchecked.hashMix = function (x, y) {
        return (x << 5) + x + y;
    };
    Unchecked.Compare = function (a, b) {
        var $1, m, $2, cmp, k, k$1;
        if (a === b)
            return 0;
        else {
            m = typeof a;
            switch (m == "function" ? 1 : m == "boolean" ? 2 : m == "number" ? 2 : m == "string" ? 2 : m == "object" ? 3 : 0) {
                case 0:
                    return typeof b == "undefined" ? 0 : -1;
                case 1:
                    return Operators.FailWith("Cannot compare function values.");
                case 2:
                    return a < b ? -1 : 1;
                case 3:
                    if (a === null)
                        $2 = -1;
                    else
                        if (b === null)
                            $2 = 1;
                        else
                            if ("CompareTo" in a)
                                $2 = a.CompareTo(b);
                            else
                                if ("CompareTo0" in a)
                                    $2 = a.CompareTo0(b);
                                else
                                    if (a instanceof Global.Array && b instanceof Global.Array)
                                        $2 = Unchecked.compareArrays(a, b);
                                    else
                                        if (a instanceof Global.Date && b instanceof Global.Date)
                                            $2 = Unchecked.compareDates(a, b);
                                        else {
                                            cmp = [0];
                                            for (var k$2 in a) if (function (k$3) {
                                                return !a.hasOwnProperty(k$3) ? false : !b.hasOwnProperty(k$3) ? (cmp[0] = 1, true) : (cmp[0] = Unchecked.Compare(a[k$3], b[k$3]), cmp[0] !== 0);
                                            }(k$2))
                                                break;
                                            if (cmp[0] === 0) {
                                                for (var k$3 in b) if (function (k$4) {
                                                    return !b.hasOwnProperty(k$4) ? false : !a.hasOwnProperty(k$4) && (cmp[0] = -1, true);
                                                }(k$3))
                                                    break;
                                            }
                                            $2 = cmp[0];
                                        }
                    return $2;
            }
        }
    };
    Unchecked.compareArrays = function (a, b) {
        var cmp, i;
        if (Arrays.length(a) < Arrays.length(b))
            return -1;
        else
            if (Arrays.length(a) > Arrays.length(b))
                return 1;
            else {
                cmp = 0;
                i = 0;
                while (cmp === 0 && i < Arrays.length(a)) {
                    cmp = Unchecked.Compare(Arrays.get(a, i), Arrays.get(b, i));
                    i = i + 1;
                }
                return cmp;
            }
    };
    Unchecked.compareDates = function (a, b) {
        return Unchecked.Compare(a.getTime(), b.getTime());
    };
    Dictionary = Collections.Dictionary = Runtime.Class({
        set_Item: function (k, v) {
            this.set(k, v);
        },
        ContainsKey: function (k) {
            var $this, d;
            $this = this;
            d = this.data[this.hash(k)];
            return d == null ? false : Arrays.exists(function (a) {
                return $this.equals.apply(null, [(Operators.KeyValue(a))[0], k]);
            }, d);
        },
        TryGetValue: function (k, res) {
            var $this, d, v;
            $this = this;
            d = this.data[this.hash(k)];
            return d == null ? false : (v = Arrays.tryPick(function (a) {
                var a$1;
                a$1 = Operators.KeyValue(a);
                return $this.equals.apply(null, [a$1[0], k]) ? {
                    $: 1,
                    $0: a$1[1]
                } : null;
            }, d), v != null && v.$ == 1 && (res.set(v.$0), true));
        },
        RemoveKey: function (k) {
            return this.remove(k);
        },
        Keys: function () {
            return new KeyCollection.New(this);
        },
        set: function (k, v) {
            var $this, h, d, m;
            $this = this;
            h = this.hash(k);
            d = this.data[h];
            if (d == null) {
                this.count = this.count + 1;
                this.data[h] = new Global.Array({
                    K: k,
                    V: v
                });
            }
            else {
                m = Arrays.tryFindIndex(function (a) {
                    return $this.equals.apply(null, [(Operators.KeyValue(a))[0], k]);
                }, d);
                m == null ? (this.count = this.count + 1, d.push({
                    K: k,
                    V: v
                })) : d[m.$0] = {
                    K: k,
                    V: v
                };
            }
        },
        remove: function (k) {
            var $this, h, d, r;
            $this = this;
            h = this.hash(k);
            d = this.data[h];
            return d == null ? false : (r = Arrays.filter(function (a) {
                return !$this.equals.apply(null, [(Operators.KeyValue(a))[0], k]);
            }, d), Arrays.length(r) < d.length && (this.count = this.count - 1, this.data[h] = r, true));
        },
        Item: function (k) {
            return this.get(k);
        },
        get: function (k) {
            var $this, d;
            $this = this;
            d = this.data[this.hash(k)];
            return d == null ? DictionaryUtil.notPresent() : Arrays.pick(function (a) {
                var a$1;
                a$1 = Operators.KeyValue(a);
                return $this.equals.apply(null, [a$1[0], k]) ? {
                    $: 1,
                    $0: a$1[1]
                } : null;
            }, d);
        },
        GetEnumerator: function () {
            return Enumerator.Get0(Arrays.concat(JS.GetFieldValues(this.data)));
        }
    }, Obj, Dictionary);
    Dictionary.New$5 = Runtime.Ctor(function () {
        Dictionary.New$6.call(this, [], Unchecked.Equals, Unchecked.Hash);
    }, Dictionary);
    Dictionary.New$6 = Runtime.Ctor(function (init, equals, hash) {
        var e, x;
        Obj.New.call(this);
        this.equals = equals;
        this.hash = hash;
        this.count = 0;
        this.data = [];
        e = Enumerator.Get(init);
        try {
            while (e.MoveNext()) {
                x = e.Current();
                this.set(x.K, x.V);
            }
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    }, Dictionary);
    Fresh.Id = function () {
        (Fresh.lastId())[0]++;
        return "Form" + Global.String((Fresh.lastId())[0]);
    };
    Fresh.lastId = function () {
        SC$3.$cctor();
        return SC$3.lastId;
    };
    ConcreteVar = UI.ConcreteVar = Runtime.Class({
        Get: function () {
            return this.current;
        },
        Set: function (v) {
            if (this.isConst)
                (function ($1) {
                    return $1("WebSharper.UI: invalid attempt to change value of a Var after calling SetFinal");
                }(function (s) {
                    console.log(s);
                }));
            else {
                Snap.Obsolete(this.snap);
                this.current = v;
                this.snap = Snap.New({
                    $: 2,
                    $0: v,
                    $1: []
                });
            }
        },
        get_View: function () {
            return this.view;
        },
        get_Id: function () {
            return "uinref" + Global.String(this.id);
        },
        UpdateMaybe: function (f) {
            var m;
            m = f(this.Get());
            if (m != null && m.$ == 1)
                this.Set(m.$0);
        }
    }, Var, ConcreteVar);
    ConcreteVar.New = Runtime.Ctor(function (isConst, initSnap, initValue) {
        var $this;
        $this = this;
        Var.New.call(this);
        this.isConst = isConst;
        this.current = initValue;
        this.snap = initSnap;
        this.view = function () {
            return $this.snap;
        };
        this.id = Fresh$1.Int();
    }, ConcreteVar);
    Snap.Obsolete = function (sn) {
        var $1, m, i, $2, o;
        m = sn.s;
        if (m == null || (m != null && m.$ == 2 ? ($1 = m.$1, false) : m != null && m.$ == 3 ? ($1 = m.$1, false) : true))
            void 0;
        else {
            sn.s = null;
            for (i = 0, $2 = Arrays.length($1) - 1; i <= $2; i++) {
                o = Arrays.get($1, i);
                if (typeof o == "object")
                    (function (sn$1) {
                        Snap.Obsolete(sn$1);
                    }(o));
                else
                    o();
            }
        }
    };
    Snap.New = function (State) {
        return {
            s: State
        };
    };
    Docs.CreateEmbedNode = function () {
        return {
            Current: null,
            Dirty: false
        };
    };
    Docs.UpdateEmbedNode = function (node, upd) {
        node.Current = upd;
        node.Dirty = true;
    };
    Docs.LinkElement = function (el, children) {
        Docs.InsertDoc(el, children, null);
    };
    Docs.InsertDoc = function (parent, doc, pos) {
        var d, b, a;
        while (true)
            if (doc != null && doc.$ == 1)
                return Docs.InsertNode(parent, doc.$0.El, pos);
            else
                if (doc != null && doc.$ == 2) {
                    d = doc.$0;
                    d.Dirty = false;
                    doc = d.Current;
                }
                else
                    if (doc == null)
                        return pos;
                    else
                        if (doc != null && doc.$ == 4)
                            return Docs.InsertNode(parent, doc.$0.Text, pos);
                        else
                            if (doc != null && doc.$ == 5)
                                return Docs.InsertNode(parent, doc.$0, pos);
                            else
                                if (doc != null && doc.$ == 6)
                                    return Arrays.foldBack(function ($1, $2) {
                                        return (((Runtime.Curried3(function (parent$1, el, pos$1) {
                                            return el == null || el.constructor === Object ? Docs.InsertDoc(parent$1, el, pos$1) : Docs.InsertNode(parent$1, el, pos$1);
                                        }))(parent))($1))($2);
                                    }, doc.$0.Els, pos);
                                else {
                                    b = doc.$1;
                                    a = doc.$0;
                                    doc = a;
                                    pos = Docs.InsertDoc(parent, b, pos);
                                }
    };
    Docs.CreateRunState = function (parent, doc) {
        return RunState.New(NodeSet.get_Empty(), Docs.CreateElemNode(parent, Attrs.EmptyAttr(), doc));
    };
    Docs.PerformAnimatedUpdate = function (childrenOnly, st, doc) {
        var _;
        return An.get_UseAnimations() ? (_ = null, Concurrency.Delay(function () {
            var cur, change, enter;
            cur = NodeSet.FindAll(doc);
            change = Docs.ComputeChangeAnim(st, cur);
            enter = Docs.ComputeEnterAnim(st, cur);
            return Concurrency.Bind(An.Play(An.Append(change, Docs.ComputeExitAnim(st, cur))), function () {
                return Concurrency.Bind(Docs.SyncElemNodesNextFrame(childrenOnly, st), function () {
                    return Concurrency.Bind(An.Play(enter), function () {
                        st.PreviousNodes = cur;
                        return Concurrency.Return(null);
                    });
                });
            });
        })) : Docs.SyncElemNodesNextFrame(childrenOnly, st);
    };
    Docs.PerformSyncUpdate = function (childrenOnly, st, doc) {
        var cur;
        cur = NodeSet.FindAll(doc);
        Docs.SyncElemNode(childrenOnly, st.Top);
        st.PreviousNodes = cur;
    };
    Docs.CreateElemNode = function (el, attr$1, children) {
        var attr$2;
        Docs.LinkElement(el, children);
        attr$2 = Attrs.Insert(el, attr$1);
        return DocElemNode.New(attr$2, children, null, el, Fresh$1.Int(), Runtime.GetOptional(attr$2.OnAfterRender));
    };
    Docs.InsertNode = function (parent, node, pos) {
        DomUtility.InsertAt(parent, pos, node);
        return node;
    };
    Docs.SyncElemNodesNextFrame = function (childrenOnly, st) {
        function a(ok) {
            Global.requestAnimationFrame(function () {
                Docs.SyncElemNode(childrenOnly, st.Top);
                ok();
            });
        }
        return Settings.BatchUpdatesEnabled() ? Concurrency.FromContinuations(function ($1, $2, $3) {
            return a.apply(null, [$1, $2, $3]);
        }) : (Docs.SyncElemNode(childrenOnly, st.Top), Concurrency.Return(null));
    };
    Docs.ComputeExitAnim = function (st, cur) {
        return An.Concat(Arrays.map(function (n) {
            return Attrs.GetExitAnim(n.Attr);
        }, NodeSet.ToArray(NodeSet.Except(cur, NodeSet.Filter(function (n) {
            return Attrs.HasExitAnim(n.Attr);
        }, st.PreviousNodes)))));
    };
    Docs.ComputeEnterAnim = function (st, cur) {
        return An.Concat(Arrays.map(function (n) {
            return Attrs.GetEnterAnim(n.Attr);
        }, NodeSet.ToArray(NodeSet.Except(st.PreviousNodes, NodeSet.Filter(function (n) {
            return Attrs.HasEnterAnim(n.Attr);
        }, cur)))));
    };
    Docs.ComputeChangeAnim = function (st, cur) {
        var relevant;
        function a(n) {
            return Attrs.HasChangeAnim(n.Attr);
        }
        relevant = function (a$1) {
            return NodeSet.Filter(a, a$1);
        };
        return An.Concat(Arrays.map(function (n) {
            return Attrs.GetChangeAnim(n.Attr);
        }, NodeSet.ToArray(NodeSet.Intersect(relevant(st.PreviousNodes), relevant(cur)))));
    };
    Docs.SyncElemNode = function (childrenOnly, el) {
        !childrenOnly ? Docs.SyncElement(el) : void 0;
        Docs.Sync(el.Children);
        Docs.AfterRender(el);
    };
    Docs.SyncElement = function (el) {
        function hasDirtyChildren(el$1) {
            function dirty(doc) {
                var t, b, a, d;
                while (true) {
                    if (doc != null && doc.$ == 0) {
                        b = doc.$1;
                        a = doc.$0;
                        if (dirty(a))
                            return true;
                        else
                            doc = b;
                    }
                    else
                        if (doc != null && doc.$ == 2) {
                            d = doc.$0;
                            if (d.Dirty)
                                return true;
                            else
                                doc = d.Current;
                        }
                        else
                            return doc != null && doc.$ == 6 && (t = doc.$0, t.Dirty || Arrays.exists(hasDirtyChildren, t.Holes));
                }
            }
            return dirty(el$1.Children);
        }
        Attrs.Sync(el.El, el.Attr);
        if (hasDirtyChildren(el))
            Docs.DoSyncElement(el);
    };
    Docs.Sync = function (doc) {
        var d, t, n, b, a;
        while (true) {
            if (doc != null && doc.$ == 1)
                return Docs.SyncElemNode(false, doc.$0);
            else
                if (doc != null && doc.$ == 2) {
                    n = doc.$0;
                    doc = n.Current;
                }
                else
                    if (doc == null)
                        return null;
                    else
                        if (doc != null && doc.$ == 5)
                            return null;
                        else
                            if (doc != null && doc.$ == 4) {
                                d = doc.$0;
                                return d.Dirty ? (d.Text.nodeValue = d.Value, d.Dirty = false) : null;
                            }
                            else
                                if (doc != null && doc.$ == 6) {
                                    t = doc.$0;
                                    Arrays.iter(function (h) {
                                        Docs.SyncElemNode(false, h);
                                    }, t.Holes);
                                    Arrays.iter(function (t$1) {
                                        Attrs.Sync(t$1[0], t$1[1]);
                                    }, t.Attrs);
                                    return Docs.AfterRender(t);
                                }
                                else {
                                    b = doc.$1;
                                    a = doc.$0;
                                    Docs.Sync(a);
                                    doc = b;
                                }
        }
    };
    Docs.AfterRender = function (el) {
        var m;
        m = Runtime.GetOptional(el.Render);
        if (m != null && m.$ == 1) {
            m.$0(el.El);
            Runtime.SetOptional(el, "Render", null);
        }
    };
    Docs.DoSyncElement = function (el) {
        var parent, p, m;
        function ins(doc, pos) {
            var t, d, b, a;
            while (true) {
                if (doc != null && doc.$ == 1)
                    return doc.$0.El;
                else
                    if (doc != null && doc.$ == 2) {
                        d = doc.$0;
                        if (d.Dirty) {
                            d.Dirty = false;
                            return Docs.InsertDoc(parent, d.Current, pos);
                        }
                        else
                            doc = d.Current;
                    }
                    else
                        if (doc == null)
                            return pos;
                        else
                            if (doc != null && doc.$ == 4)
                                return doc.$0.Text;
                            else
                                if (doc != null && doc.$ == 5)
                                    return doc.$0;
                                else
                                    if (doc != null && doc.$ == 6) {
                                        t = doc.$0;
                                        t.Dirty ? t.Dirty = false : void 0;
                                        return Arrays.foldBack(function ($1, $2) {
                                            return $1 == null || $1.constructor === Object ? ins($1, $2) : $1;
                                        }, t.Els, pos);
                                    }
                                    else {
                                        b = doc.$1;
                                        a = doc.$0;
                                        doc = a;
                                        pos = ins(b, pos);
                                    }
            }
        }
        parent = el.El;
        DomNodes.Iter((p = el.El, function (e) {
            DomUtility.RemoveNode(p, e);
        }), DomNodes.Except(DomNodes.DocChildren(el), DomNodes.Children(el.El, Runtime.GetOptional(el.Delimiters))));
        ins(el.Children, (m = Runtime.GetOptional(el.Delimiters), m != null && m.$ == 1 ? m.$0[1] : null));
    };
    Attrs.Static = function (attr$1) {
        return new AttrProxy({
            $: 3,
            $0: attr$1
        });
    };
    Attrs.Updates = function (dyn) {
        return Array.MapTreeReduce(function (x) {
            return x.NChanged();
        }, View.Const(), View.Map2Unit, dyn.DynNodes);
    };
    Attrs.AppendTree = function (a, b) {
        var x;
        return a === null ? b : b === null ? a : (x = new AttrProxy({
            $: 2,
            $0: a,
            $1: b
        }), (Attrs.SetFlags(x, Attrs.Flags(a) | Attrs.Flags(b)), x));
    };
    Attrs.EmptyAttr = function () {
        SC$4.$cctor();
        return SC$4.EmptyAttr;
    };
    Attrs.Dynamic = function (view, set) {
        return new AttrProxy({
            $: 1,
            $0: new DynamicAttrNode.New(view, set)
        });
    };
    Attrs.Insert = function (elem, tree) {
        var nodes, oar, arr;
        function loop(node) {
            var b, a;
            while (true)
                if (!(node === null)) {
                    if (node != null && node.$ == 1)
                        return nodes.push(node.$0);
                    else
                        if (node != null && node.$ == 2) {
                            b = node.$1;
                            a = node.$0;
                            loop(a);
                            node = b;
                        }
                        else
                            return node != null && node.$ == 3 ? node.$0(elem) : node != null && node.$ == 4 ? oar.push(node.$0) : null;
                }
                else
                    return null;
        }
        nodes = [];
        oar = [];
        loop(tree);
        arr = nodes.slice(0);
        return Dyn.New(elem, Attrs.Flags(tree), arr, oar.length === 0 ? null : {
            $: 1,
            $0: function (el) {
                Seq.iter(function (f) {
                    f(el);
                }, oar);
            }
        });
    };
    Attrs.SetFlags = function (a, f) {
        a.flags = f;
    };
    Attrs.Flags = function (a) {
        return a !== null && a.hasOwnProperty("flags") ? a.flags : 0;
    };
    Attrs.HasExitAnim = function (attr$1) {
        var flag;
        flag = 2;
        return (attr$1.DynFlags & flag) === flag;
    };
    Attrs.GetExitAnim = function (dyn) {
        return Attrs.GetAnim(dyn, function ($1, $2) {
            return $1.NGetExitAnim($2);
        });
    };
    Attrs.HasEnterAnim = function (attr$1) {
        var flag;
        flag = 1;
        return (attr$1.DynFlags & flag) === flag;
    };
    Attrs.GetEnterAnim = function (dyn) {
        return Attrs.GetAnim(dyn, function ($1, $2) {
            return $1.NGetEnterAnim($2);
        });
    };
    Attrs.HasChangeAnim = function (attr$1) {
        var flag;
        flag = 4;
        return (attr$1.DynFlags & flag) === flag;
    };
    Attrs.GetChangeAnim = function (dyn) {
        return Attrs.GetAnim(dyn, function ($1, $2) {
            return $1.NGetChangeAnim($2);
        });
    };
    Attrs.GetAnim = function (dyn, f) {
        return An.Concat(Arrays.map(function (n) {
            return f(n, dyn.DynElem);
        }, dyn.DynNodes));
    };
    Attrs.Sync = function (elem, dyn) {
        Arrays.iter(function (d) {
            d.NSync(elem);
        }, dyn.DynNodes);
    };
    DomUtility.ParseHTMLIntoFakeRoot = function (elem) {
        var root, tag, m, p, w;
        function unwrap(elt, a) {
            var i;
            while (true)
                if (a === 0)
                    return elt;
                else {
                    i = a;
                    elt = elt.lastChild;
                    a = i - 1;
                }
        }
        root = self.document.createElement("div");
        return !DomUtility.rhtml().test(elem) ? (root.appendChild(self.document.createTextNode(elem)), root) : (tag = (m = DomUtility.rtagName().exec(elem), Unchecked.Equals(m, null) ? "" : Arrays.get(m, 1).toLowerCase()), (p = (w = (DomUtility.wrapMap())[tag], w ? w : DomUtility.defaultWrap()), (root.innerHTML = p[1] + elem.replace(DomUtility.rxhtmlTag(), "<$1></$2>") + p[2], unwrap(root, p[0]))));
    };
    DomUtility.rhtml = function () {
        SC$7.$cctor();
        return SC$7.rhtml;
    };
    DomUtility.wrapMap = function () {
        SC$7.$cctor();
        return SC$7.wrapMap;
    };
    DomUtility.defaultWrap = function () {
        SC$7.$cctor();
        return SC$7.defaultWrap;
    };
    DomUtility.rxhtmlTag = function () {
        SC$7.$cctor();
        return SC$7.rxhtmlTag;
    };
    DomUtility.rtagName = function () {
        SC$7.$cctor();
        return SC$7.rtagName;
    };
    DomUtility.IterSelector = function (el, selector, f) {
        var l, i, $1;
        l = el.querySelectorAll(selector);
        for (i = 0, $1 = l.length - 1; i <= $1; i++)f(l[i]);
    };
    DomUtility.InsertAt = function (parent, pos, node) {
        var m;
        if (!(node.parentNode === parent && pos === (m = node.nextSibling, Unchecked.Equals(m, null) ? null : m)))
            parent.insertBefore(node, pos);
    };
    DomUtility.RemoveNode = function (parent, el) {
        if (el.parentNode === parent)
            parent.removeChild(el);
    };
    AttrModule.Value = function (_var) {
        return AttrModule.ValueWith(BindVar.StringApply(), _var);
    };
    AttrModule.ValueWith = function (bind, _var) {
        var p;
        p = bind(_var);
        return AttrProxy.Append(Attrs.Static(p[0]), AttrModule.DynamicCustom(p[1], p[2]));
    };
    AttrModule.DynamicCustom = function (set, view) {
        return Attrs.Dynamic(view, set);
    };
    Array.ofSeqNonCopying = function (xs) {
        var q, o;
        if (xs instanceof Global.Array)
            return xs;
        else
            if (xs instanceof T)
                return Arrays.ofList(xs);
            else
                if (xs === null)
                    return [];
                else {
                    q = [];
                    o = Enumerator.Get(xs);
                    try {
                        while (o.MoveNext())
                            q.push(o.Current());
                        return q;
                    }
                    finally {
                        if (typeof o == "object" && "Dispose" in o)
                            o.Dispose();
                    }
                }
    };
    Array.TreeReduce = function (defaultValue, reduction, array) {
        var l;
        function loop(off) {
            return function (len) {
                var $1, l2;
                switch (len <= 0 ? 0 : len === 1 ? off >= 0 && off < l ? 1 : ($1 = len, 2) : ($1 = len, 2)) {
                    case 0:
                        return defaultValue;
                    case 1:
                        return Arrays.get(array, off);
                    case 2:
                        l2 = len / 2 >> 0;
                        return reduction((loop(off))(l2), (loop(off + l2))(len - l2));
                }
            };
        }
        l = Arrays.length(array);
        return (loop(0))(l);
    };
    Array.MapTreeReduce = function (mapping, defaultValue, reduction, array) {
        var l;
        function loop(off) {
            return function (len) {
                var $1, l2;
                switch (len <= 0 ? 0 : len === 1 ? off >= 0 && off < l ? 1 : ($1 = len, 2) : ($1 = len, 2)) {
                    case 0:
                        return defaultValue;
                    case 1:
                        return mapping(Arrays.get(array, off));
                    case 2:
                        l2 = len / 2 >> 0;
                        return reduction((loop(off))(l2), (loop(off + l2))(len - l2));
                }
            };
        }
        l = Arrays.length(array);
        return (loop(0))(l);
    };
    Enumerator.Get = function (x) {
        return x instanceof Global.Array ? Enumerator.ArrayEnumerator(x) : Unchecked.Equals(typeof x, "string") ? Enumerator.StringEnumerator(x) : x.GetEnumerator();
    };
    Enumerator.ArrayEnumerator = function (s) {
        return new T$1.New(0, null, function (e) {
            var i;
            i = e.s;
            return i < Arrays.length(s) && (e.c = Arrays.get(s, i), e.s = i + 1, true);
        }, void 0);
    };
    Enumerator.StringEnumerator = function (s) {
        return new T$1.New(0, null, function (e) {
            var i;
            i = e.s;
            return i < s.length && (e.c = s[i], e.s = i + 1, true);
        }, void 0);
    };
    Enumerator.Get0 = function (x) {
        return x instanceof Global.Array ? Enumerator.ArrayEnumerator(x) : Unchecked.Equals(typeof x, "string") ? Enumerator.StringEnumerator(x) : "GetEnumerator0" in x ? x.GetEnumerator0() : x.GetEnumerator();
    };
    T$1 = Enumerator.T = Runtime.Class({
        Dispose: function () {
            if (this.d)
                this.d(this);
        },
        MoveNext: function () {
            var m;
            m = this.n(this);
            this.e = m ? 1 : 2;
            return m;
        },
        Current: function () {
            return this.e === 1 ? this.c : this.e === 0 ? Operators.FailWith("Enumeration has not started. Call MoveNext.") : Operators.FailWith("Enumeration already finished.");
        }
    }, Obj, T$1);
    T$1.New = Runtime.Ctor(function (s, c, n, d) {
        Obj.New.call(this);
        this.s = s;
        this.c = c;
        this.n = n;
        this.d = d;
        this.e = 0;
    }, T$1);
    Arrays.get = function (arr, n) {
        Arrays.checkBounds(arr, n);
        return arr[n];
    };
    Arrays.length = function (arr) {
        return arr.dims === 2 ? arr.length * arr.length : arr.length;
    };
    Arrays.checkBounds = function (arr, n) {
        if (n < 0 || n >= arr.length)
            Operators.FailWith("Index was outside the bounds of the array.");
    };
    Arrays.set = function (arr, n, x) {
        Arrays.checkBounds(arr, n);
        arr[n] = x;
    };
    SC$2.$cctor = function () {
        SC$2.$cctor = Global.ignore;
        SC$2.LoadedTemplates = new Dictionary.New$5();
        SC$2.LocalTemplatesLoaded = false;
        SC$2.GlobalHoles = new Dictionary.New$5();
        SC$2.TextHoleRE = "\\${([^}]+)}";
        SC$2.RenderedFullDocTemplate = null;
    };
    HashSet = Collections.HashSet = Runtime.Class({
        SAdd: function (item) {
            return this.add(item);
        },
        Contains: function (item) {
            var arr;
            arr = this.data[this.hash(item)];
            return arr == null ? false : this.arrContains(item, arr);
        },
        add: function (item) {
            var h, arr;
            h = this.hash(item);
            arr = this.data[h];
            return arr == null ? (this.data[h] = [item], this.count = this.count + 1, true) : this.arrContains(item, arr) ? false : (arr.push(item), this.count = this.count + 1, true);
        },
        arrContains: function (item, arr) {
            var c, i, $1, l;
            c = true;
            i = 0;
            l = arr.length;
            while (c && i < l)
                if (this.equals.apply(null, [arr[i], item]))
                    c = false;
                else
                    i = i + 1;
            return !c;
        },
        GetEnumerator: function () {
            return Enumerator.Get(HashSetUtil.concat(this.data));
        },
        ExceptWith: function (xs) {
            var e;
            e = Enumerator.Get(xs);
            try {
                while (e.MoveNext())
                    this.Remove(e.Current());
            }
            finally {
                if (typeof e == "object" && "Dispose" in e)
                    e.Dispose();
            }
        },
        Count: function () {
            return this.count;
        },
        IntersectWith: function (xs) {
            var other, all, i, $1, item;
            other = new HashSet.New$4(xs, this.equals, this.hash);
            all = HashSetUtil.concat(this.data);
            for (i = 0, $1 = all.length - 1; i <= $1; i++) {
                item = all[i];
                if (!other.Contains(item))
                    this.Remove(item);
            }
        },
        Remove: function (item) {
            var arr;
            arr = this.data[this.hash(item)];
            return arr == null ? false : this.arrRemove(item, arr) && (this.count = this.count - 1, true);
        },
        CopyTo: function (arr, index) {
            var all, i, $1;
            all = HashSetUtil.concat(this.data);
            for (i = 0, $1 = all.length - 1; i <= $1; i++)Arrays.set(arr, i + index, all[i]);
        },
        arrRemove: function (item, arr) {
            var c, i, $1, l;
            c = true;
            i = 0;
            l = arr.length;
            while (c && i < l)
                if (this.equals.apply(null, [arr[i], item])) {
                    arr.splice.apply(arr, [i, 1]);
                    c = false;
                }
                else
                    i = i + 1;
            return !c;
        }
    }, Obj, HashSet);
    HashSet.New$3 = Runtime.Ctor(function () {
        HashSet.New$4.call(this, [], Unchecked.Equals, Unchecked.Hash);
    }, HashSet);
    HashSet.New$4 = Runtime.Ctor(function (init, equals, hash) {
        var e;
        Obj.New.call(this);
        this.equals = equals;
        this.hash = hash;
        this.data = [];
        this.count = 0;
        e = Enumerator.Get(init);
        try {
            while (e.MoveNext())
                this.add(e.Current());
        }
        finally {
            if (typeof e == "object" && "Dispose" in e)
                e.Dispose();
        }
    }, HashSet);
    HashSet.New$2 = Runtime.Ctor(function (init) {
        HashSet.New$4.call(this, init, Unchecked.Equals, Unchecked.Hash);
    }, HashSet);
    Fresh$1.Int = function () {
        Fresh$1.set_counter(Fresh$1.counter() + 1);
        return Fresh$1.counter();
    };
    Fresh$1.set_counter = function ($1) {
        SC$6.$cctor();
        SC$6.counter = $1;
    };
    Fresh$1.counter = function () {
        SC$6.$cctor();
        return SC$6.counter;
    };
    DocElemNode = UI.DocElemNode = Runtime.Class({
        Equals: function (o) {
            return this.ElKey === o.ElKey;
        },
        GetHashCode: function () {
            return this.ElKey;
        }
    }, null, DocElemNode);
    DocElemNode.New = function (Attr, Children, Delimiters, El, ElKey, Render) {
        var $1;
        return new DocElemNode(($1 = {
            Attr: Attr,
            Children: Children,
            El: El,
            ElKey: ElKey
        }, (Runtime.SetOptional($1, "Delimiters", Delimiters), Runtime.SetOptional($1, "Render", Render), $1)));
    };
    BindVar.StringApply = function () {
        SC$4.$cctor();
        return SC$4.StringApply;
    };
    BindVar.ApplyValue = function (get, set, _var) {
        var expectedValue;
        function f(a, o) {
            return o == null ? null : a(o.$0);
        }
        expectedValue = null;
        return [function (el) {
            function onChange() {
                _var.UpdateMaybe(function (v) {
                    var $1;
                    expectedValue = get(el);
                    return expectedValue != null && expectedValue.$ == 1 && (!Unchecked.Equals(expectedValue.$0, v) && ($1 = [expectedValue, expectedValue.$0], true)) ? $1[0] : null;
                });
            }
            el.addEventListener("change", onChange);
            el.addEventListener("input", onChange);
            el.addEventListener("keypress", onChange);
        }, function (x) {
            var $1;
            $1 = set(x);
            return function ($2) {
                return f($1, $2);
            };
        }, View.Map(function (v) {
            var $1;
            return expectedValue != null && expectedValue.$ == 1 && (Unchecked.Equals(expectedValue.$0, v) && ($1 = expectedValue.$0, true)) ? null : {
                $: 1,
                $0: v
            };
        }, _var.get_View())];
    };
    BindVar.StringSet = function () {
        SC$4.$cctor();
        return SC$4.StringSet;
    };
    BindVar.StringGet = function () {
        SC$4.$cctor();
        return SC$4.StringGet;
    };
    BindVar.DateTimeSetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.DateTimeSetUnchecked;
    };
    BindVar.DateTimeGetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.DateTimeGetUnchecked;
    };
    BindVar.FileApplyValue = function (get, set, _var) {
        var expectedValue;
        function f(a, o) {
            return o == null ? null : a(o.$0);
        }
        expectedValue = null;
        return [function (el) {
            el.addEventListener("change", function () {
                _var.UpdateMaybe(function (v) {
                    var $1;
                    expectedValue = get(el);
                    return expectedValue != null && expectedValue.$ == 1 && (expectedValue.$0 !== v && ($1 = [expectedValue, expectedValue.$0], true)) ? $1[0] : null;
                });
            });
        }, function (x) {
            var $1;
            $1 = set(x);
            return function ($2) {
                return f($1, $2);
            };
        }, View.Map(function (v) {
            var $1;
            return expectedValue != null && expectedValue.$ == 1 && (Unchecked.Equals(expectedValue.$0, v) && ($1 = expectedValue.$0, true)) ? null : {
                $: 1,
                $0: v
            };
        }, _var.get_View())];
    };
    BindVar.FileSetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.FileSetUnchecked;
    };
    BindVar.FileGetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.FileGetUnchecked;
    };
    BindVar.IntSetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.IntSetUnchecked;
    };
    BindVar.IntGetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.IntGetUnchecked;
    };
    BindVar.IntSetChecked = function () {
        SC$4.$cctor();
        return SC$4.IntSetChecked;
    };
    BindVar.IntGetChecked = function () {
        SC$4.$cctor();
        return SC$4.IntGetChecked;
    };
    BindVar.FloatSetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.FloatSetUnchecked;
    };
    BindVar.FloatGetUnchecked = function () {
        SC$4.$cctor();
        return SC$4.FloatGetUnchecked;
    };
    BindVar.FloatSetChecked = function () {
        SC$4.$cctor();
        return SC$4.FloatSetChecked;
    };
    BindVar.FloatGetChecked = function () {
        SC$4.$cctor();
        return SC$4.FloatGetChecked;
    };
    Arrays.ofList = function (xs) {
        var l, q;
        q = [];
        l = xs;
        while (!(l.$ == 0)) {
            q.push(List.head(l));
            l = List.tail(l);
        }
        return q;
    };
    Arrays.exists = function (f, x) {
        var e, i, $1, l;
        e = false;
        i = 0;
        l = Arrays.length(x);
        while (!e && i < l)
            if (f(x[i]))
                e = true;
            else
                i = i + 1;
        return e;
    };
    Arrays.tryPick = function (f, arr) {
        var res, i, m;
        res = null;
        i = 0;
        while (i < arr.length && res == null) {
            m = f(arr[i]);
            if (m != null && m.$ == 1)
                res = m;
            i = i + 1;
        }
        return res;
    };
    Arrays.tryFindIndex = function (f, arr) {
        var res, i;
        res = null;
        i = 0;
        while (i < arr.length && res == null) {
            f(arr[i]) ? res = {
                $: 1,
                $0: i
            } : void 0;
            i = i + 1;
        }
        return res;
    };
    Arrays.filter = function (f, arr) {
        var r, i, $1;
        r = [];
        for (i = 0, $1 = arr.length - 1; i <= $1; i++)if (f(arr[i]))
            r.push(arr[i]);
        return r;
    };
    Arrays.map = function (f, arr) {
        var r, i, $1;
        r = new Global.Array(arr.length);
        for (i = 0, $1 = arr.length - 1; i <= $1; i++)r[i] = f(arr[i]);
        return r;
    };
    Arrays.iter = function (f, arr) {
        var i, $1;
        for (i = 0, $1 = arr.length - 1; i <= $1; i++)f(arr[i]);
    };
    Arrays.foldBack = function (f, arr, zero) {
        var acc, $1, len, i, $2;
        acc = zero;
        len = arr.length;
        for (i = 1, $2 = len; i <= $2; i++)acc = f(arr[len - i], acc);
        return acc;
    };
    Arrays.pick = function (f, arr) {
        var m;
        m = Arrays.tryPick(f, arr);
        return m == null ? Operators.FailWith("KeyNotFoundException") : m.$0;
    };
    Arrays.concat = function (xs) {
        return Global.Array.prototype.concat.apply([], Arrays.ofSeq(xs));
    };
    Arrays.ofSeq = function (xs) {
        var q, o;
        if (xs instanceof Global.Array)
            return xs.slice();
        else
            if (xs instanceof T)
                return Arrays.ofList(xs);
            else {
                q = [];
                o = Enumerator.Get(xs);
                try {
                    while (o.MoveNext())
                        q.push(o.Current());
                    return q;
                }
                finally {
                    if (typeof o == "object" && "Dispose" in o)
                        o.Dispose();
                }
            }
    };
    Arrays.choose = function (f, arr) {
        var q, i, $1, m;
        q = [];
        for (i = 0, $1 = arr.length - 1; i <= $1; i++) {
            m = f(arr[i]);
            if (m == null)
                ;
            else
                q.push(m.$0);
        }
        return q;
    };
    Arrays.create = function (size, value) {
        var r, i, $1;
        r = new Global.Array(size);
        for (i = 0, $1 = size - 1; i <= $1; i++)r[i] = value;
        return r;
    };
    Arrays.init = function (size, f) {
        var r, i, $1;
        if (size < 0)
            Operators.FailWith("Negative size given.");
        else
            null;
        r = new Global.Array(size);
        for (i = 0, $1 = size - 1; i <= $1; i++)r[i] = f(i);
        return r;
    };
    Arrays.forall = function (f, x) {
        var a, i, $1, l;
        a = true;
        i = 0;
        l = Arrays.length(x);
        while (a && i < l)
            if (f(x[i]))
                i = i + 1;
            else
                a = false;
        return a;
    };
    Elt = UI.Elt = Runtime.Class({}, Doc, Elt);
    Elt.New = function (el, attr$1, children) {
        var node, rvUpdates;
        node = Docs.CreateElemNode(el, attr$1, children.docNode);
        rvUpdates = Updates.Create(children.updates);
        return new Elt.New$1({
            $: 1,
            $0: node
        }, View.Map2Unit(Attrs.Updates(node.Attr), rvUpdates.v), el, rvUpdates);
    };
    Elt.New$1 = Runtime.Ctor(function (docNode, updates, elt, rvUpdates) {
        Doc.New.call(this, docNode, updates);
        this.docNode$1 = docNode;
        this.updates$1 = updates;
        this.elt = elt;
        this.rvUpdates = rvUpdates;
    }, Elt);
    DictionaryUtil.notPresent = function () {
        throw new KeyNotFoundException.New();
    };
    Prepare.convertTextNode = function (n) {
        var m, li, $1, s, strRE, hole;
        m = null;
        li = 0;
        s = n.textContent;
        strRE = new Global.RegExp(Templates.TextHoleRE(), "g");
        while (m = strRE.exec(s), m !== null) {
            n.parentNode.insertBefore(self.document.createTextNode(Slice.string(s, {
                $: 1,
                $0: li
            }, {
                $: 1,
                $0: strRE.lastIndex - Arrays.get(m, 0).length - 1
            })), n);
            li = strRE.lastIndex;
            hole = self.document.createElement("span");
            hole.setAttribute("ws-replace", Arrays.get(m, 1).toLowerCase());
            n.parentNode.insertBefore(hole, n);
        }
        strRE.lastIndex = 0;
        n.textContent = Slice.string(s, {
            $: 1,
            $0: li
        }, null);
    };
    Prepare.failNotLoaded = function (name) {
        console.warn("Instantiating non-loaded template", name);
    };
    Prepare.fillTextHole = function (instance, fillWith, templateName) {
        var m;
        m = instance.querySelector("[ws-replace]");
        return Unchecked.Equals(m, null) ? (console.warn("Filling non-existent text hole", templateName), null) : (m.parentNode.replaceChild(self.document.createTextNode(fillWith), m), {
            $: 1,
            $0: m.getAttribute("ws-replace")
        });
    };
    Prepare.removeHolesExcept = function (instance, dontRemove) {
        function run(attrName) {
            Templates.foreachNotPreserved(instance, "[" + attrName + "]", function (e) {
                if (!dontRemove.Contains(e.getAttribute(attrName)))
                    e.removeAttribute(attrName);
            });
        }
        run("ws-attr");
        run("ws-onafterrender");
        run("ws-var");
        Templates.foreachNotPreserved(instance, "[ws-hole]", function (e) {
            if (!dontRemove.Contains(e.getAttribute("ws-hole"))) {
                e.removeAttribute("ws-hole");
                while (e.hasChildNodes())
                    e.removeChild(e.lastChild);
            }
        });
        Templates.foreachNotPreserved(instance, "[ws-replace]", function (e) {
            if (!dontRemove.Contains(e.getAttribute("ws-replace")))
                e.parentNode.removeChild(e);
        });
        Templates.foreachNotPreserved(instance, "[ws-on]", function (e) {
            e.setAttribute("ws-on", Strings.concat(" ", Arrays.filter(function (x) {
                return dontRemove.Contains(Arrays.get(Strings.SplitChars(x, [":"], 1), 1));
            }, Strings.SplitChars(e.getAttribute("ws-on"), [" "], 1))));
        });
        Templates.foreachNotPreserved(instance, "[ws-attr-holes]", function (e) {
            var holeAttrs, i, $1, attrName, _this;
            holeAttrs = Strings.SplitChars(e.getAttribute("ws-attr-holes"), [" "], 1);
            for (i = 0, $1 = holeAttrs.length - 1; i <= $1; i++) {
                attrName = Arrays.get(holeAttrs, i);
                e.setAttribute(attrName, (_this = new Global.RegExp(Templates.TextHoleRE(), "g"), e.getAttribute(attrName).replace(_this, function ($2, $3) {
                    return dontRemove.Contains($3) ? $2 : "";
                })));
            }
        });
    };
    Prepare.fillInstanceAttrs = function (instance, fillWith) {
        var name, m, i, $1, a;
        Prepare.convertAttrs(fillWith);
        name = fillWith.nodeName.toLowerCase();
        m = instance.querySelector("[ws-attr=" + name + "]");
        if (Unchecked.Equals(m, null))
            console.warn("Filling non-existent attr hole", name);
        else {
            m.removeAttribute("ws-attr");
            for (i = 0, $1 = fillWith.attributes.length - 1; i <= $1; i++) {
                a = fillWith.attributes.item(i);
                if (a.name == "class" && m.hasAttribute("class"))
                    m.setAttribute("class", m.getAttribute("class") + " " + a.nodeValue);
                else
                    m.setAttribute(a.name, a.nodeValue);
            }
        }
    };
    Prepare.mapHoles = function (t, mappings) {
        function run(attrName) {
            Templates.foreachNotPreserved(t, "[" + attrName + "]", function (e) {
                var m, o;
                m = (o = null, [mappings.TryGetValue(e.getAttribute(attrName).toLowerCase(), {
                    get: function () {
                        return o;
                    },
                    set: function (v) {
                        o = v;
                    }
                }), o]);
                if (m[0])
                    e.setAttribute(attrName, m[1]);
            });
        }
        run("ws-hole");
        run("ws-replace");
        run("ws-attr");
        run("ws-onafterrender");
        run("ws-var");
        Templates.foreachNotPreserved(t, "[ws-on]", function (e) {
            e.setAttribute("ws-on", Strings.concat(" ", Arrays.map(function (x) {
                var a, m, o;
                a = Strings.SplitChars(x, [":"], 1);
                m = (o = null, [mappings.TryGetValue(Arrays.get(a, 1), {
                    get: function () {
                        return o;
                    },
                    set: function (v) {
                        o = v;
                    }
                }), o]);
                return m[0] ? Arrays.get(a, 0) + ":" + m[1] : x;
            }, Strings.SplitChars(e.getAttribute("ws-on"), [" "], 1))));
        });
        Templates.foreachNotPreserved(t, "[ws-attr-holes]", function (e) {
            var holeAttrs, i, $1;
            holeAttrs = Strings.SplitChars(e.getAttribute("ws-attr-holes"), [" "], 1);
            for (i = 0, $1 = holeAttrs.length - 1; i <= $1; i++)(function () {
                var attrName;
                function f(s, a) {
                    var a$1;
                    a$1 = Operators.KeyValue(a);
                    return s.replace(new Global.RegExp("\\${" + a$1[0] + "}", "ig"), "${" + a$1[1] + "}");
                }
                attrName = Arrays.get(holeAttrs, i);
                return e.setAttribute(attrName, (((Runtime.Curried3(Seq.fold))(f))(e.getAttribute(attrName)))(mappings));
            }());
        });
    };
    Prepare.fill = function (fillWith, p, n) {
        while (true)
            if (fillWith.hasChildNodes())
                n = p.insertBefore(fillWith.lastChild, n);
            else
                return null;
    };
    Prepare.convertAttrs = function (el) {
        var attrs, toRemove, events, holedAttrs, i, $1, a, _this;
        function lowercaseAttr(name) {
            var m;
            m = el.getAttribute(name);
            if (m == null)
                ;
            else
                el.setAttribute(name, m.toLowerCase());
        }
        attrs = el.attributes;
        toRemove = [];
        events = [];
        holedAttrs = [];
        for (i = 0, $1 = attrs.length - 1; i <= $1; i++) {
            a = attrs.item(i);
            if (Strings.StartsWith(a.nodeName, "ws-on") && a.nodeName != "ws-onafterrender" && a.nodeName != "ws-on") {
                toRemove.push(a.nodeName);
                events.push(Slice.string(a.nodeName, {
                    $: 1,
                    $0: "ws-on".length
                }, null) + ":" + a.nodeValue.toLowerCase());
            }
            else
                !Strings.StartsWith(a.nodeName, "ws-") && (new Global.RegExp(Templates.TextHoleRE())).test(a.nodeValue) ? (a.nodeValue = (_this = new Global.RegExp(Templates.TextHoleRE(), "g"), a.nodeValue.replace(_this, function ($2, $3) {
                    return "${" + $3.toLowerCase() + "}";
                })), holedAttrs.push(a.nodeName)) : void 0;
        }
        if (!(events.length == 0))
            el.setAttribute("ws-on", Strings.concat(" ", events));
        if (!(holedAttrs.length == 0))
            el.setAttribute("ws-attr-holes", Strings.concat(" ", holedAttrs));
        lowercaseAttr("ws-hole");
        lowercaseAttr("ws-replace");
        lowercaseAttr("ws-attr");
        lowercaseAttr("ws-onafterrender");
        lowercaseAttr("ws-var");
        Arrays.iter(function (a$1) {
            el.removeAttribute(a$1);
        }, toRemove);
    };
    Slice.string = function (source, start, finish) {
        var f, f$1;
        return start == null ? finish != null && finish.$ == 1 ? (f = finish.$0, f < 0 ? "" : source.slice(0, f + 1)) : "" : finish == null ? source.slice(start.$0) : (f$1 = finish.$0, f$1 < 0 ? "" : source.slice(start.$0, f$1 + 1));
    };
    Seq.insufficient = function () {
        return Operators.FailWith("The input sequence has an insufficient number of elements.");
    };
    Seq.nonNegative = function () {
        return Operators.FailWith("The input must be non-negative.");
    };
    KeyCollection = Collections.KeyCollection = Runtime.Class({
        GetEnumerator: function () {
            return Enumerator.Get(Seq.map(function (kvp) {
                return kvp.K;
            }, this.d));
        }
    }, Obj, KeyCollection);
    KeyCollection.New = Runtime.Ctor(function (d) {
        Obj.New.call(this);
        this.d = d;
    }, KeyCollection);
    Numeric.TryParseInt32 = function (s, r) {
        return Numeric.TryParse(s, -2147483648, 2147483647, r);
    };
    An.get_UseAnimations = function () {
        return Anims.UseAnimations();
    };
    An.Play = function (anim) {
        var _;
        _ = null;
        return Concurrency.Delay(function () {
            return Concurrency.Bind(An.Run(Global.ignore, Anims.Actions(anim)), function () {
                Anims.Finalize(anim);
                return Concurrency.Return(null);
            });
        });
    };
    An.Append = function (a, a$1) {
        return {
            $: 0,
            $0: AppendList.Append(a.$0, a$1.$0)
        };
    };
    An.Run = function (k, anim) {
        var dur;
        function a(ok) {
            function loop(start) {
                return function (now) {
                    var t;
                    t = now - start;
                    anim.Compute(t);
                    k();
                    return t <= dur ? void Global.requestAnimationFrame(function (t$1) {
                        (loop(start))(t$1);
                    }) : ok();
                };
            }
            Global.requestAnimationFrame(function (t) {
                (loop(t))(t);
            });
        }
        dur = anim.Duration;
        return dur === 0 ? Concurrency.Zero() : Concurrency.FromContinuations(function ($1, $2, $3) {
            return a.apply(null, [$1, $2, $3]);
        });
    };
    An.Concat = function (xs) {
        return {
            $: 0,
            $0: AppendList.Concat(Seq.map(Anims.List, xs))
        };
    };
    An.get_Empty = function () {
        return {
            $: 0,
            $0: AppendList.Empty()
        };
    };
    Settings.BatchUpdatesEnabled = function () {
        SC$5.$cctor();
        return SC$5.BatchUpdatesEnabled;
    };
    Mailbox.StartProcessor = function (procAsync) {
        var st;
        function work() {
            var _;
            _ = null;
            return Concurrency.Delay(function () {
                return Concurrency.Bind(procAsync, function () {
                    var m;
                    m = st[0];
                    return Unchecked.Equals(m, 1) ? (st[0] = 0, Concurrency.Zero()) : Unchecked.Equals(m, 2) ? (st[0] = 1, work()) : Concurrency.Zero();
                });
            });
        }
        st = [0];
        return function () {
            var m;
            m = st[0];
            if (Unchecked.Equals(m, 0)) {
                st[0] = 1;
                Concurrency.Start(work(), null);
            }
            else
                Unchecked.Equals(m, 1) ? st[0] = 2 : void 0;
        };
    };
    SC$3.$cctor = function () {
        var x;
        SC$3.$cctor = Global.ignore;
        SC$3.lastId = [0];
        SC$3.Int = (x = [0], function () {
            x[0]++;
            return x[0];
        });
    };
    Dyn.New = function (DynElem, DynFlags, DynNodes, OnAfterRender) {
        var $1;
        $1 = {
            DynElem: DynElem,
            DynFlags: DynFlags,
            DynNodes: DynNodes
        };
        Runtime.SetOptional($1, "OnAfterRender", OnAfterRender);
        return $1;
    };
    SC$4.$cctor = function () {
        var g, s, g$1, s$1, g$2, s$2, g$3, s$3, g$4, s$4, g$5, s$5, g$6, s$6;
        SC$4.$cctor = Global.ignore;
        SC$4.EmptyAttr = null;
        SC$4.BoolCheckedApply = function (_var) {
            function set(el, v) {
                return v != null && v.$ == 1 ? void (el.checked = v.$0) : null;
            }
            return [function (el) {
                el.addEventListener("change", function () {
                    return _var.Get() != el.checked ? _var.Set(el.checked) : null;
                });
            }, function ($1) {
                return function ($2) {
                    return set($1, $2);
                };
            }, View.Map(function (a) {
                return {
                    $: 1,
                    $0: a
                };
            }, _var.get_View())];
        };
        SC$4.StringSet = function (el) {
            return function (s$7) {
                el.value = s$7;
            };
        };
        SC$4.StringGet = function (el) {
            return {
                $: 1,
                $0: el.value
            };
        };
        SC$4.StringApply = (g = BindVar.StringGet(), (s = BindVar.StringSet(), function (v) {
            return BindVar.ApplyValue(g, s, v);
        }));
        SC$4.DateTimeSetUnchecked = function (el) {
            return function (i) {
                el.value = (new Global.Date(i)).toLocaleString();
            };
        };
        SC$4.DateTimeGetUnchecked = function (el) {
            var s$7, m, o, m$1;
            s$7 = el.value;
            return String.isBlank(s$7) ? {
                $: 1,
                $0: -8640000000000000
            } : (m = (o = 0, [(m$1 = DateUtil.TryParse(s$7), m$1 != null && m$1.$ == 1 && (o = m$1.$0, true)), o]), m[0] ? {
                $: 1,
                $0: m[1]
            } : null);
        };
        SC$4.DateTimeApplyUnchecked = (g$1 = BindVar.DateTimeGetUnchecked(), (s$1 = BindVar.DateTimeSetUnchecked(), function (v) {
            return BindVar.ApplyValue(g$1, s$1, v);
        }));
        SC$4.FileSetUnchecked = function () {
            return function () {
                return null;
            };
        };
        SC$4.FileGetUnchecked = function (el) {
            var files;
            files = el.files;
            return {
                $: 1,
                $0: Arrays.ofSeq(Seq.delay(function () {
                    return Seq.map(function (i) {
                        return files.item(i);
                    }, Operators.range(0, files.length - 1));
                }))
            };
        };
        SC$4.FileApplyUnchecked = (g$2 = BindVar.FileGetUnchecked(), (s$2 = BindVar.FileSetUnchecked(), function (v) {
            return BindVar.FileApplyValue(g$2, s$2, v);
        }));
        SC$4.IntSetUnchecked = function (el) {
            return function (i) {
                el.value = Global.String(i);
            };
        };
        SC$4.IntGetUnchecked = function (el) {
            var s$7, pd;
            s$7 = el.value;
            return String.isBlank(s$7) ? {
                $: 1,
                $0: 0
            } : (pd = +s$7, pd !== pd >> 0 ? null : {
                $: 1,
                $0: pd
            });
        };
        SC$4.IntApplyUnchecked = (g$3 = BindVar.IntGetUnchecked(), (s$3 = BindVar.IntSetUnchecked(), function (v) {
            return BindVar.ApplyValue(g$3, s$3, v);
        }));
        SC$4.IntSetChecked = function (el) {
            return function (i) {
                var i$1;
                i$1 = i.get_Input();
                return el.value != i$1 ? void (el.value = i$1) : null;
            };
        };
        SC$4.IntGetChecked = function (el) {
            var s$7, m, o;
            s$7 = el.value;
            return {
                $: 1,
                $0: String.isBlank(s$7) ? (el.checkValidity ? el.checkValidity() : true) ? new CheckedInput({
                    $: 2,
                    $0: s$7
                }) : new CheckedInput({
                    $: 1,
                    $0: s$7
                }) : (m = (o = 0, [Numeric.TryParseInt32(s$7, {
                    get: function () {
                        return o;
                    },
                    set: function (v) {
                        o = v;
                    }
                }), o]), m[0] ? new CheckedInput({
                    $: 0,
                    $0: m[1],
                    $1: s$7
                }) : new CheckedInput({
                    $: 1,
                    $0: s$7
                }))
            };
        };
        SC$4.IntApplyChecked = (g$4 = BindVar.IntGetChecked(), (s$4 = BindVar.IntSetChecked(), function (v) {
            return BindVar.ApplyValue(g$4, s$4, v);
        }));
        SC$4.FloatSetUnchecked = function (el) {
            return function (i) {
                el.value = Global.String(i);
            };
        };
        SC$4.FloatGetUnchecked = function (el) {
            var s$7, pd;
            s$7 = el.value;
            return String.isBlank(s$7) ? {
                $: 1,
                $0: 0
            } : (pd = +s$7, Global.isNaN(pd) ? null : {
                $: 1,
                $0: pd
            });
        };
        SC$4.FloatApplyUnchecked = (g$5 = BindVar.FloatGetUnchecked(), (s$5 = BindVar.FloatSetUnchecked(), function (v) {
            return BindVar.ApplyValue(g$5, s$5, v);
        }));
        SC$4.FloatSetChecked = function (el) {
            return function (i) {
                var i$1;
                i$1 = i.get_Input();
                return el.value != i$1 ? void (el.value = i$1) : null;
            };
        };
        SC$4.FloatGetChecked = function (el) {
            var s$7, i;
            s$7 = el.value;
            return {
                $: 1,
                $0: String.isBlank(s$7) ? (el.checkValidity ? el.checkValidity() : true) ? new CheckedInput({
                    $: 2,
                    $0: s$7
                }) : new CheckedInput({
                    $: 1,
                    $0: s$7
                }) : (i = +s$7, Global.isNaN(i) ? new CheckedInput({
                    $: 1,
                    $0: s$7
                }) : new CheckedInput({
                    $: 0,
                    $0: i,
                    $1: s$7
                }))
            };
        };
        SC$4.FloatApplyChecked = (g$6 = BindVar.FloatGetChecked(), (s$6 = BindVar.FloatSetChecked(), function (v) {
            return BindVar.ApplyValue(g$6, s$6, v);
        }));
    };
    Updates = UI.Updates = Runtime.Class({}, null, Updates);
    Updates.Create = function (v) {
        var _var;
        _var = null;
        _var = Updates.New(v, null, function () {
            var c;
            c = _var.s;
            return c === null ? (c = Snap.Copy(_var.c()), _var.s = c, Snap.WhenObsoleteRun(c, function () {
                _var.s = null;
            }), c) : c;
        });
        return _var;
    };
    Updates.New = function (Current, Snap$1, VarView) {
        return new Updates({
            c: Current,
            s: Snap$1,
            v: VarView
        });
    };
    Strings.concat = function (separator, strings) {
        return Arrays.ofSeq(strings).join(separator);
    };
    Strings.SplitChars = function (s, sep, opts) {
        return Strings.Split(s, new Global.RegExp("[" + Strings.RegexEscape(sep.join("")) + "]"), opts);
    };
    Strings.StartsWith = function (t, s) {
        return t.substring(0, s.length) == s;
    };
    Strings.Split = function (s, pat, opts) {
        return opts === 1 ? Arrays.filter(function (x) {
            return x !== "";
        }, Strings.SplitWith(s, pat)) : Strings.SplitWith(s, pat);
    };
    Strings.RegexEscape = function (s) {
        return s.replace(new Global.RegExp("[-\\/\\\\^$*+?.()|[\\]{}]", "g"), "\\$&");
    };
    Strings.forall = function (f, s) {
        return Seq.forall(f, Strings.protect(s));
    };
    Strings.SplitWith = function (str, pat) {
        return str.split(pat);
    };
    Strings.protect = function (s) {
        return s == null ? "" : s;
    };
    RunState.New = function (PreviousNodes, Top) {
        return {
            PreviousNodes: PreviousNodes,
            Top: Top
        };
    };
    NodeSet.get_Empty = function () {
        return {
            $: 0,
            $0: new HashSet.New$3()
        };
    };
    NodeSet.FindAll = function (doc) {
        var q;
        function recF(recI, $1) {
            var x, b, a, el, em;
            while (true)
                switch (recI) {
                    case 0:
                        if ($1 != null && $1.$ == 0) {
                            b = $1.$1;
                            a = $1.$0;
                            recF(0, a);
                            $1 = b;
                        }
                        else
                            if ($1 != null && $1.$ == 1) {
                                el = $1.$0;
                                $1 = el;
                                recI = 1;
                            }
                            else
                                if ($1 != null && $1.$ == 2) {
                                    em = $1.$0;
                                    $1 = em.Current;
                                }
                                else
                                    return $1 != null && $1.$ == 6 ? (x = $1.$0.Holes, (function (a$1) {
                                        return function (a$2) {
                                            Arrays.iter(a$1, a$2);
                                        };
                                    }(loopEN))(x)) : null;
                        break;
                    case 1:
                        q.push($1);
                        $1 = $1.Children;
                        recI = 0;
                        break;
                }
        }
        function loop(node) {
            return recF(0, node);
        }
        function loopEN(el) {
            return recF(1, el);
        }
        q = [];
        loop(doc);
        return {
            $: 0,
            $0: new HashSet.New$2(q)
        };
    };
    NodeSet.Filter = function (f, a) {
        return {
            $: 0,
            $0: HashSet$1.Filter(f, a.$0)
        };
    };
    NodeSet.Except = function (a, a$1) {
        return {
            $: 0,
            $0: HashSet$1.Except(a.$0, a$1.$0)
        };
    };
    NodeSet.ToArray = function (a) {
        return HashSet$1.ToArray(a.$0);
    };
    NodeSet.Intersect = function (a, a$1) {
        return {
            $: 0,
            $0: HashSet$1.Intersect(a.$0, a$1.$0)
        };
    };
    Concurrency.Delay = function (mk) {
        return function (c) {
            try {
                (mk(null))(c);
            }
            catch (e) {
                c.k({
                    $: 1,
                    $0: e
                });
            }
        };
    };
    Concurrency.Bind = function (r, f) {
        return Concurrency.checkCancel(function (c) {
            r(AsyncBody.New(function (a) {
                var x;
                if (a.$ == 0) {
                    x = a.$0;
                    Concurrency.scheduler().Fork(function () {
                        try {
                            (f(x))(c);
                        }
                        catch (e) {
                            c.k({
                                $: 1,
                                $0: e
                            });
                        }
                    });
                }
                else
                    Concurrency.scheduler().Fork(function () {
                        c.k(a);
                    });
            }, c.ct));
        });
    };
    Concurrency.Zero = function () {
        SC$9.$cctor();
        return SC$9.Zero;
    };
    Concurrency.Start = function (c, ctOpt) {
        var ct, d;
        ct = (d = (Concurrency.defCTS())[0], ctOpt == null ? d : ctOpt.$0);
        Concurrency.scheduler().Fork(function () {
            if (!ct.c)
                c(AsyncBody.New(function (a) {
                    if (a.$ == 1)
                        Concurrency.UncaughtAsyncError(a.$0);
                }, ct));
        });
    };
    Concurrency.Return = function (x) {
        return function (c) {
            c.k({
                $: 0,
                $0: x
            });
        };
    };
    Concurrency.scheduler = function () {
        SC$9.$cctor();
        return SC$9.scheduler;
    };
    Concurrency.checkCancel = function (r) {
        return function (c) {
            if (c.ct.c)
                Concurrency.cancel(c);
            else
                r(c);
        };
    };
    Concurrency.defCTS = function () {
        SC$9.$cctor();
        return SC$9.defCTS;
    };
    Concurrency.UncaughtAsyncError = function (e) {
        console.log("WebSharper: Uncaught asynchronous exception", e);
    };
    Concurrency.FromContinuations = function (subscribe) {
        return function (c) {
            var continued;
            function once(cont) {
                if (continued[0])
                    Operators.FailWith("A continuation provided by Async.FromContinuations was invoked multiple times");
                else {
                    continued[0] = true;
                    Concurrency.scheduler().Fork(cont);
                }
            }
            continued = [false];
            subscribe(function (a) {
                once(function () {
                    c.k({
                        $: 0,
                        $0: a
                    });
                });
            }, function (e) {
                once(function () {
                    c.k({
                        $: 1,
                        $0: e
                    });
                });
            }, function (e) {
                once(function () {
                    c.k({
                        $: 2,
                        $0: e
                    });
                });
            });
        };
    };
    Concurrency.cancel = function (c) {
        c.k({
            $: 2,
            $0: new OperationCanceledException.New(c.ct)
        });
    };
    Anims.UseAnimations = function () {
        SC$8.$cctor();
        return SC$8.UseAnimations;
    };
    Anims.Actions = function (a) {
        return Anims.ConcatActions(Arrays.choose(function (a$1) {
            return a$1.$ == 1 ? {
                $: 1,
                $0: a$1.$0
            } : null;
        }, AppendList.ToArray(a.$0)));
    };
    Anims.Finalize = function (a) {
        Arrays.iter(function (a$1) {
            if (a$1.$ == 0)
                a$1.$0();
        }, AppendList.ToArray(a.$0));
    };
    Anims.ConcatActions = function (xs) {
        var xs$1, m, dur, xs$2;
        xs$1 = Array.ofSeqNonCopying(xs);
        m = Arrays.length(xs$1);
        return m === 0 ? Anims.Const() : m === 1 ? Arrays.get(xs$1, 0) : (dur = Seq.max(Seq.map(function (anim) {
            return anim.Duration;
        }, xs$1)), (xs$2 = Arrays.map(function (x) {
            return Anims.Prolong(dur, x);
        }, xs$1), Anims.Def(dur, function (t) {
            Arrays.iter(function (anim) {
                anim.Compute(t);
            }, xs$2);
        })));
    };
    Anims.List = function (a) {
        return a.$0;
    };
    Anims.Const = function (v) {
        return Anims.Def(0, function () {
            return v;
        });
    };
    Anims.Def = function (d, f) {
        return {
            Compute: f,
            Duration: d
        };
    };
    Anims.Prolong = function (nextDuration, anim) {
        var comp, dur, last;
        comp = anim.Compute;
        dur = anim.Duration;
        last = Lazy.Create(function () {
            return anim.Compute(anim.Duration);
        });
        return {
            Compute: function (t) {
                return t >= dur ? last.f() : comp(t);
            },
            Duration: nextDuration
        };
    };
    SC$5.$cctor = function () {
        SC$5.$cctor = Global.ignore;
        SC$5.BatchUpdatesEnabled = true;
    };
    SC$6.$cctor = function () {
        SC$6.$cctor = Global.ignore;
        SC$6.counter = 0;
    };
    Queue.Clear = function (a) {
        a.splice(0, Arrays.length(a));
    };
    String.isBlank = function (s) {
        return Strings.forall(Char.IsWhiteSpace, s);
    };
    CheckedInput = UI.CheckedInput = Runtime.Class({
        get_Input: function () {
            return this.$ == 1 ? this.$0 : this.$ == 2 ? this.$0 : this.$1;
        }
    }, null, CheckedInput);
    SC$7.$cctor = function () {
        var table;
        SC$7.$cctor = Global.ignore;
        SC$7.rxhtmlTag = new Global.RegExp("<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\\w:]+)[^>]*)\\/>", "gi");
        SC$7.rtagName = new Global.RegExp("<([\\w:]+)");
        SC$7.rhtml = new Global.RegExp("<|&#?\\w+;");
        SC$7.wrapMap = (table = [1, "<table>", "</table>"], {
            option: [1, "<select multiple='multiple'>", "</select>"],
            legend: [1, "<fieldset>", "</fieldset>"],
            area: [1, "<map>", "</map>"],
            param: [1, "<object>", "</object>"],
            thead: table,
            tbody: table,
            tfoot: table,
            tr: [2, "<table><tbody>", "</tbody></table>"],
            col: [2, "<table><colgroup>", "</colgoup></table>"],
            td: [3, "<table><tbody><tr>", "</tr></tbody></table>"]
        });
        SC$7.defaultWrap = [0, "", ""];
    };
    SC$8.$cctor = function () {
        SC$8.$cctor = Global.ignore;
        SC$8.CubicInOut = Easing.Custom(function (t) {
            var t2;
            t2 = t * t;
            return 3 * t2 - 2 * (t2 * t);
        });
        SC$8.UseAnimations = true;
    };
    AppendList.Append = function (x, y) {
        return x.$ == 0 ? y : y.$ == 0 ? x : {
            $: 2,
            $0: x,
            $1: y
        };
    };
    AppendList.ToArray = function (xs) {
        var out;
        function loop(xs$1) {
            var y, x;
            while (true)
                if (xs$1.$ == 1)
                    return out.push(xs$1.$0);
                else
                    if (xs$1.$ == 2) {
                        y = xs$1.$1;
                        x = xs$1.$0;
                        loop(x);
                        xs$1 = y;
                    }
                    else
                        return xs$1.$ == 3 ? Arrays.iter(function (v) {
                            out.push(v);
                        }, xs$1.$0) : null;
        }
        out = [];
        loop(xs);
        return out.slice(0);
    };
    AppendList.Concat = function (xs) {
        var x;
        x = Array.ofSeqNonCopying(xs);
        return Array.TreeReduce(AppendList.Empty(), AppendList.Append, x);
    };
    AppendList.Empty = function () {
        SC$10.$cctor();
        return SC$10.Empty;
    };
    HashSetUtil.concat = function (o) {
        var r, k;
        r = [];
        for (var k$1 in o) r.push.apply(r, o[k$1]);
        return r;
    };
    DynamicAttrNode = UI.DynamicAttrNode = Runtime.Class({
        NChanged: function () {
            return this.updates;
        },
        NGetExitAnim: function (parent) {
            return An.get_Empty();
        },
        NGetEnterAnim: function (parent) {
            return An.get_Empty();
        },
        NGetChangeAnim: function (parent) {
            return An.get_Empty();
        },
        NSync: function (parent) {
            if (this.dirty) {
                (this.push(parent))(this.value);
                this.dirty = false;
            }
        }
    }, Obj, DynamicAttrNode);
    DynamicAttrNode.New = Runtime.Ctor(function (view, push) {
        var $this;
        $this = this;
        Obj.New.call(this);
        this.push = push;
        this.value = void 0;
        this.dirty = false;
        this.updates = View.Map(function (x) {
            $this.value = x;
            $this.dirty = true;
        }, view);
    }, DynamicAttrNode);
    Char.IsWhiteSpace = function (c) {
        return c.match(new Global.RegExp("\\s")) !== null;
    };
    DateUtil.TryParse = function (s) {
        var d;
        d = Date.parse(s);
        return Global.isNaN(d) ? null : {
            $: 1,
            $0: d
        };
    };
    Numeric.TryParse = function (s, min, max, r) {
        var x, ok;
        x = +s;
        ok = x === x - x % 1 && x >= min && x <= max;
        if (ok)
            r.set(x);
        return ok;
    };
    KeyNotFoundException = WebSharper.KeyNotFoundException = Runtime.Class({}, Error, KeyNotFoundException);
    KeyNotFoundException.New = Runtime.Ctor(function () {
        KeyNotFoundException.New$1.call(this, "The given key was not present in the dictionary.");
    }, KeyNotFoundException);
    KeyNotFoundException.New$1 = Runtime.Ctor(function (message) {
        this.message = message;
        Object.setPrototypeOf(this, KeyNotFoundException.prototype);
    }, KeyNotFoundException);
    Scheduler = Concurrency.Scheduler = Runtime.Class({
        Fork: function (action) {
            var $this;
            $this = this;
            this.robin.push(action);
            if (this.idle) {
                this.idle = false;
                Global.setTimeout(function () {
                    $this.tick();
                }, 0);
            }
        },
        tick: function () {
            var loop, $this, t;
            $this = this;
            t = Date.now();
            loop = true;
            while (loop)
                if (this.robin.length === 0) {
                    this.idle = true;
                    loop = false;
                }
                else {
                    (this.robin.shift())();
                    Date.now() - t > 40 ? (Global.setTimeout(function () {
                        $this.tick();
                    }, 0), loop = false) : void 0;
                }
        }
    }, Obj, Scheduler);
    Scheduler.New = Runtime.Ctor(function () {
        Obj.New.call(this);
        this.idle = true;
        this.robin = [];
    }, Scheduler);
    Easing = UI.Easing = Runtime.Class({}, Obj, Easing);
    Easing.Custom = function (f) {
        return new Easing.New(f);
    };
    Easing.New = Runtime.Ctor(function (transformTime) {
        Obj.New.call(this);
        this.transformTime = transformTime;
    }, Easing);
    AsyncBody.New = function (k, ct) {
        return {
            k: k,
            ct: ct
        };
    };
    SC$9.$cctor = function () {
        SC$9.$cctor = Global.ignore;
        SC$9.noneCT = CT.New(false, []);
        SC$9.scheduler = new Scheduler.New();
        SC$9.defCTS = [new CancellationTokenSource.New()];
        SC$9.Zero = Concurrency.Return();
        SC$9.GetCT = function (c) {
            c.k({
                $: 0,
                $0: c.ct
            });
        };
    };
    CT.New = function (IsCancellationRequested, Registrations) {
        return {
            c: IsCancellationRequested,
            r: Registrations
        };
    };
    HashSet$1.Filter = function (ok, set) {
        return new HashSet.New$2(Arrays.filter(ok, HashSet$1.ToArray(set)));
    };
    HashSet$1.Except = function (excluded, included) {
        var set;
        set = new HashSet.New$2(HashSet$1.ToArray(included));
        set.ExceptWith(HashSet$1.ToArray(excluded));
        return set;
    };
    HashSet$1.ToArray = function (set) {
        var arr;
        arr = Arrays.create(set.Count(), void 0);
        set.CopyTo(arr, 0);
        return arr;
    };
    HashSet$1.Intersect = function (a, b) {
        var set;
        set = new HashSet.New$2(HashSet$1.ToArray(a));
        set.IntersectWith(HashSet$1.ToArray(b));
        return set;
    };
    CancellationTokenSource = WebSharper.CancellationTokenSource = Runtime.Class({}, Obj, CancellationTokenSource);
    CancellationTokenSource.New = Runtime.Ctor(function () {
        Obj.New.call(this);
        this.c = false;
        this.pending = null;
        this.r = [];
        this.init = 1;
    }, CancellationTokenSource);
    DomNodes.Children = function (elem, delims) {
        var n, o, a;
        if (delims != null && delims.$ == 1) {
            a = [];
            n = delims.$0[0].nextSibling;
            while (n !== delims.$0[1]) {
                a.push(n);
                n = n.nextSibling;
            }
            return {
                $: 0,
                $0: a
            };
        }
        else
            return {
                $: 0,
                $0: Arrays.init(elem.childNodes.length, (o = elem.childNodes, function (a$1) {
                    return o[a$1];
                }))
            };
    };
    DomNodes.Except = function (a, a$1) {
        var excluded;
        excluded = a.$0;
        return {
            $: 0,
            $0: Arrays.filter(function (n) {
                return Arrays.forall(function (k) {
                    return !(n === k);
                }, excluded);
            }, a$1.$0)
        };
    };
    DomNodes.Iter = function (f, a) {
        Arrays.iter(f, a.$0);
    };
    DomNodes.DocChildren = function (node) {
        var q;
        function loop(doc) {
            var x, d, b, a;
            while (true) {
                if (doc != null && doc.$ == 2) {
                    d = doc.$0;
                    doc = d.Current;
                }
                else
                    if (doc != null && doc.$ == 1)
                        return q.push(doc.$0.El);
                    else
                        if (doc == null)
                            return null;
                        else
                            if (doc != null && doc.$ == 5)
                                return q.push(doc.$0);
                            else
                                if (doc != null && doc.$ == 4)
                                    return q.push(doc.$0.Text);
                                else
                                    if (doc != null && doc.$ == 6) {
                                        x = doc.$0.Els;
                                        return (function (a$1) {
                                            return function (a$2) {
                                                Arrays.iter(a$1, a$2);
                                            };
                                        }(function (a$1) {
                                            if (a$1 == null || a$1.constructor === Object)
                                                loop(a$1);
                                            else
                                                q.push(a$1);
                                        }))(x);
                                    }
                                    else {
                                        b = doc.$1;
                                        a = doc.$0;
                                        loop(a);
                                        doc = b;
                                    }
            }
        }
        q = [];
        loop(node.Children);
        return {
            $: 0,
            $0: Array.ofSeqNonCopying(q)
        };
    };
    OperationCanceledException = WebSharper.OperationCanceledException = Runtime.Class({}, Error, OperationCanceledException);
    OperationCanceledException.New = Runtime.Ctor(function (ct) {
        OperationCanceledException.New$1.call(this, "The operation was canceled.", null, ct);
    }, OperationCanceledException);
    OperationCanceledException.New$1 = Runtime.Ctor(function (message, inner, ct) {
        this.message = message;
        this.inner = inner;
        Object.setPrototypeOf(this, OperationCanceledException.prototype);
        this.ct = ct;
    }, OperationCanceledException);
    Lazy.Create = function (f) {
        return LazyRecord.New(false, f, Lazy.forceLazy);
    };
    Lazy.forceLazy = function () {
        var v;
        v = this.v();
        this.c = true;
        this.v = v;
        this.f = Lazy.cachedLazy;
        return v;
    };
    Lazy.cachedLazy = function () {
        return this.v;
    };
    SC$10.$cctor = function () {
        SC$10.$cctor = Global.ignore;
        SC$10.Empty = {
            $: 0
        };
    };
    LazyRecord.New = function (created, evalOrVal, force) {
        return {
            c: created,
            v: evalOrVal,
            f: force
        };
    };
    Runtime.OnLoad(function () {
        ContactManager.Main();
    });
}(self));


if (typeof WebSharper !== 'undefined') {
    WebSharper.Runtime.ScriptBasePath = '/Content/';
    WebSharper.Runtime.Start();
}
