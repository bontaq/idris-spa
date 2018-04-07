"use strict";

(function(){

const $JSRTS = {
    throw: function (x) {
        throw x;
    },
    Lazy: function (e) {
        this.js_idris_lazy_calc = e;
        this.js_idris_lazy_val = void 0;
    },
    force: function (x) {
        if (x === undefined || x.js_idris_lazy_calc === undefined) {
            return x
        } else {
            if (x.js_idris_lazy_val === undefined) {
                x.js_idris_lazy_val = x.js_idris_lazy_calc()
            }
            return x.js_idris_lazy_val
        }
    },
    prim_strSubstr: function (offset, len, str) {
        return str.substr(Math.max(0, offset), Math.max(0, len))
    }
};
$JSRTS.prim_systemInfo = function (index) {
    switch (index) {
        case 0:
            return "javascript";
        case 1:
            return navigator.platform;
    }
    return "";
};
$JSRTS.prim_writeStr = function (x) { return console.log(x) }
$JSRTS.prim_readStr = function () { return prompt('Prelude.getLine') };




const $HC_0_0$MkUnit = ({type: 0});
const $HC_0_0$TheWorld = ({type: 0});
function $HC_1_1$IdrisScript__MkJSString($1){
    this.type = 1;
    this.$1 = $1;
}

// IdrisScript.log

function IdrisScript__log($_0_arg, $_1_arg, $_2_w){
    let $cg$1 = null;
    $cg$1 = $_1_arg.$1;
    return (console.log(($cg$1)));
}

// {runMain_0}

function $_0_runMain(){
    const $_4_in = IdrisScript__log(null, new $HC_1_1$IdrisScript__MkJSString("hello"), $HC_0_0$TheWorld);
    return $JSRTS.force($HC_0_0$MkUnit);
}


$_0_runMain();
}.call(this))