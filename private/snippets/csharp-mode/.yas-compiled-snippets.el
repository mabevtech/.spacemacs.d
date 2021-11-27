;;; Compiled snippets and support files for `csharp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'csharp-mode
                     '(("using" "using (var ${1:disposable} = ${2:new ${3:Disposable}($4)})\n{\n    $0\n}\n" "using (var ... = ...) { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/using" nil nil)
                       ("trycf" "try\n{\n    $0\n}\ncatch (${1:Exception} ex)\n{\n    ${2:Console.WriteLine(ex.ToString());}\n}\nfinally\n{\n    ${3:// resource}.Dispose();\n}" "try { ... } catch (ex) { ... } finally { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/trycf" nil nil)
                       ("tryc" "try\n{\n    $0\n}\ncatch (${1:Exception} ex)\n{\n    ${2:Console.WriteLine(ex.ToString());}\n}" "try { ... } catch (ex) { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/tryc" nil nil)
                       ("test" "[Fact]\npublic void ${1:Operation}_Should${2:Expectation}${3:$(if (string= yas-text \"\") \"\" \"_When\")}${3:Predicate}()\n{\n    $0\n}\n" "[Fact] public void ..._Should..._When... { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/test" nil nil)
                       ("prop" "${3:public} ${2:${1:$(yas-text)}} ${1:Prop} { get; ${4:${5:}${5:$(unless (string= yas-text \"\") \" \")}set; }}$0" "public Type Prop { get; set; }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/prop" nil nil)
                       ("nsclass" "namespace ${2:SomeNamespace}\n{\n    public class ${1:SomeClass}\n    {\n        $0\n    }\n}" "namespace { public class { ... } }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/nsclass" nil nil)
                       ("method" "${4:public} ${3:void} ${1:Method}($2)\n{\n    $0\n}\n" "public void Method(...) { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/method" nil nil)
                       ("ifelse" "if (${1:predicate})\n{\n    $2\n}\nelse\n{\n    $0\n}" "if (...) { ... } else { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/ifelse" nil nil)
                       ("if" "if (${1:predicate})\n{\n    $0\n}" "if { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/if" nil nil)
                       ("foreach" "foreach (var ${1:item} in ${2:iterable})\n{\n    $0\n}" "foreach (var item in iterable) { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/fore" nil nil)
                       ("field" "${3:public} ${2:${1:$(concat\n                       (upcase (substring yas-text 0 1))\n                       (substring yas-text 1))}} ${1:field};$0" "private Field field;" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/field" nil nil)
                       ("enum" "public enum ${1:Name}\n{\n    ${2:CategoryA},\n    ${3:CategoryB},$0\n}" "public enum ... { CategoryA, CategoryB, }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/enum" nil nil)
                       ("extm" "public static ${3:void} ${1:Method}(this ${2:Type} ${3:${2:$(concat\n                                                              (downcase (substring yas-text 0 1))\n                                                              (substring yas-text 1))}}${4:$(if (string= yas-text \"\") \"\" \", \")}${4:args})\n{\n    $0\n}" "public static void Method(this ...) { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/emethod" nil nil)
                       ("elseif" "else if (${1:predicate})\n{\n    $0\n}" "else if { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/elseif" nil nil)
                       ("else" "else\n{\n    $0\n}" "else { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/else" nil nil)
                       ("dep" "private readonly I${1:$(concat\n                         (upcase (substring yas-text 0 1))\n                         (substring yas-text 1))} _${1:component};\n" "private readonly IDep dep;" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/dep" nil nil)
                       ("cwl" "Console.WriteLine($0);" "Console.WriteLine()" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/cwl" nil nil)
                       ("cw" "Console.Write($0);" "Console.Write()" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/cw" nil nil)
                       ("class" "${3:public} class ${1:SomeClass}${2:$(if (string= yas-text \"\") \"\" \" : \")}${2:Inheritance}\n{\n    $0\n}" "class { ... }" nil nil nil "/Users/mabo3n/.emacs.d/private/snippets/csharp-mode/class" nil nil)))


;;; Do not edit! File generated at Sun Oct 31 02:48:24 2021
