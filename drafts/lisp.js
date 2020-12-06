
var defun;
var symbols = new Set();
var jsEval = eval;
if (typeof window === 'undefined') {
    var defun = function(s) {
       return function(f) {
           global[s] = f;
           symbols.add(s);
           
       };
    };
} else {
    var defun = function(s) {
       return function(f) {
           window[s] = f;
           symbols.add(s);
       };
    };

}

var define = defun;

(define ('nil') (null));

(defun
    ('test')((x) => (x + 1)));
// delet later

// convenience functions
(defun ('_typeEq') ((s) => (x) => 
    (eq (typeof x) (s))));

(defun ('display') (console.log.bind(console)));

(defun ('newline') ((s) => (display ('\n' + s))));

(defun ('iff') ((p) => (a) => (b) => 
    ((p) ? (a) : (b))));

// builtin type equality functions
(defun ('number') (_typeEq ('number')));

(defun ('char') (_typeEq ('string')));
(defun ('string') (_typeEq ('string')));

(defun ('function') (_typeEq ('function')));
(defun ('boolean') (_typeEq ('boolean')));

(defun ('symbol') ((v) => symbols.has(v)));
(defun ('array') ((v) => Array.isArray(v)));

// quoted lists

// builtin s-expr functions
(defun ('cons') ((x) => (y) => 
    ((z) => ((z)?
             (x):
             (y)))));

(defun ('car') ((f) => (f(1))));

(defun ('cdr') ((f) => (f(0))));

(defun ('fromArray') ((arr) => {
    let a = nil;
    for (let i = arr.length; i >= 0; --i) {
        a = cons(arr[i])(a);    
    }
    return a;
}));

(defun ('list') ((...varArgs) => 
    (fromArray(varArgs))));

(defun ('q') (list));

(define ('listTest') (list (1, 2, 3, 4, 5)));

(display (q([1,2,3])));
(defun ('eq') ((a) => (b) => a === b));

(defun ('map') ((f) => (xs) =>
    ((eq (xs) (nil))? 
        (nil):
        (cons (f (car (xs)))
            (map (f) (cdr (xs)))))));



(defun ('foldr') ((f) => (z) => (xs) => 
    ((eq (xs) (nil))?
        (z):
        (f (car (xs)) (foldr (f) (z) (cdr (xs)))))));

(defun ('filter') ((p) => (xs) => 
    (foldr ((x) => (a) => 
        ((p (x))?
            (cons(x)(a)):
            (a)))
        (nil) (xs))));


(define ('a') (cons(1)(cons(2)(cons(3)(nil)))));


console.log(car(a));

(define ('f') ((x) => (x + 1)));

let b = map(f)(a);
console.log(car(b));

let g = (x) => (y) => (x + y);
(display (foldr(g)(0)(a)));

(defun ('cond') ((xs) =>
    (car (car (xs))?
        (cdr (car (xs))):
        (cond (cdr (xs))))));
        
(defun ('lambda') ((...args) => (s) => 
    (jsEval
        (map ((s) => `(${s}) => `)
            (fromArray(args))))));