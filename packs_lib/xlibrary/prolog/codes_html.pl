/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(codes_html,
          [code_html/3,         % ?Code,  ?Html, ?Tail
           codes_html/2,        % +Codes, ?Html
           codes_html/3         % +Codes, ?Html, ?Tail
          ]).

codes_html(Codes, Html) :-
    codes_html(Codes, Html, []).

codes_html(Codes) -->
    foldl(code_html_nf, Codes).

code_html_nf(Code) --> code_html(Code), !.
code_html_nf(Code) --> [Code].

% escape characters taken from:
% http://www.theukwebdesigncompany.com/articles/entity-escape-characters.php

code_html(0'") --> "&quot;". % quotation mark
code_html(0'') --> "&apos;". % apostrophe
code_html(0'&) --> "&amp;". % ampersand
code_html(0'<) --> "&lt;". % less-than
code_html(0'>) --> "&gt;". % greater-than
code_html(0' ) --> "&nbsp;". % non-breaking space
code_html(0'??) --> "&iexcl;". % inverted exclamation mark
code_html(0'??) --> "&cent;". % cent
code_html(0'??) --> "&pound;". % pound sterling
code_html(0'??) --> "&curren;". % currency
code_html(0'??) --> "&yen;". % yen
code_html(0'??) --> "&brvbar;". % broken vertical bar
code_html(0'??) --> "&sect;". % section
code_html(0'??) --> "&uml;". % spacing diaeresis
code_html(0'??) --> "&copy;". % copyright
code_html(0'??) --> "&ordf;". % feminine ordinal indicator
code_html(0'??) --> "&laquo;". % angle quotation mark (left)
code_html(0'??) --> "&not;". % negation
code_html(0'&) --> "hy;". % soft hyphen
code_html(0'??) --> "&reg;". % registered trademark
code_html(0'??) --> "&macr;". % spacing macron
code_html(0'??) --> "&deg;". % degree
code_html(0'??) --> "&plusmn;". % plus-or-minus
code_html(0'??) --> "&sup2;". % superscript 2
code_html(0'??) --> "&sup3;". % superscript 3
code_html(0'??) --> "&acute;". % spacing acute
code_html(0'??) --> "&micro;". % micro
code_html(0'??) --> "&para;". % paragraph
code_html(0'??) --> "&middot;". % middle dot
code_html(0'??) --> "&cedil;". % spacing cedilla
code_html(0'??) --> "&sup1;". % superscript 1
code_html(0'??) --> "&ordm;". % masculine ordinal indicator
code_html(0'??) --> "&raquo;". % angle quotation mark (right)
code_html(0'??) --> "&frac14;". % fraction 1/4
code_html(0'??) --> "&frac12;". % fraction 1/2
code_html(0'??) --> "&frac34;". % fraction 3/4
code_html(0'??) --> "&iquest;". % inverted question mark
code_html(0'??) --> "&times;". % multiplication
code_html(0'??) --> "&divide;". % division
code_html(0'??) --> "&Agrave;". % capital a, grave accent
code_html(0'??) --> "&Aacute;". % capital a, acute accent
code_html(0'??) --> "&Acirc;". % capital a, circumflex accent
code_html(0'??) --> "&Atilde;". % capital a, tilde
code_html(0'??) --> "&Auml;". % capital a, umlaut mark
code_html(0'??) --> "&Aring;". % capital a, ring
code_html(0'??) --> "&AElig;". % capital ae
code_html(0'??) --> "&Ccedil;". % capital c, cedilla
code_html(0'??) --> "&Egrave;". % capital e, grave accent
code_html(0'??) --> "&Eacute;". % capital e, acute accent
code_html(0'??) --> "&Ecirc;". % capital e, circumflex accent
code_html(0'??) --> "&Euml;". % capital e, umlaut mark
code_html(0'??) --> "&Igrave;". % capital i, grave accent
code_html(0'??) --> "&Iacute;". % capital i, acute accent
code_html(0'??) --> "&Icirc;". % capital i, circumflex accent
code_html(0'??) --> "&Iuml;". % capital i, umlaut mark
code_html(0'??) --> "&ETH;". % capital eth, Icelandic
code_html(0'??) --> "&Ntilde;". % capital n, tilde
code_html(0'??) --> "&Ograve;". % capital o, grave accent
code_html(0'??) --> "&Oacute;". % capital o, acute accent
code_html(0'??) --> "&Ocirc;". % capital o, circumflex accent
code_html(0'??) --> "&Otilde;". % capital o, tilde
code_html(0'??) --> "&Ouml;". % capital o, umlaut mark
code_html(0'??) --> "&Oslash;". % capital o, slash
code_html(0'??) --> "&Ugrave;". % capital u, grave accent
code_html(0'??) --> "&Uacute;". % capital u, acute accent
code_html(0'??) --> "&Ucirc;". % capital u, circumflex accent
code_html(0'??) --> "&Uuml;". % capital u, umlaut mark
code_html(0'??) --> "&Yacute;". % capital y, acute accent
code_html(0'??) --> "&THORN;". % capital THORN, Icelandic
code_html(0'??) --> "&szlig;". % small sharp s, German
code_html(0'??) --> "&agrave;". % small a, grave accent
code_html(0'??) --> "&aacute;". % small a, acute accent
code_html(0'??) --> "&acirc;". % small a, circumflex accent
code_html(0'??) --> "&atilde;". % small a, tilde
code_html(0'??) --> "&auml;". % small a, umlaut mark
code_html(0'??) --> "&aring;". % small a, ring
code_html(0'??) --> "&aelig;". % small ae
code_html(0'??) --> "&ccedil;". % small c, cedilla
code_html(0'??) --> "&egrave;". % small e, grave accent
code_html(0'??) --> "&eacute;". % small e, acute accent
code_html(0'??) --> "&ecirc;". % small e, circumflex accent
code_html(0'??) --> "&euml;". % small e, umlaut mark
code_html(0'??) --> "&igrave;". % small i, grave accent
code_html(0'??) --> "&iacute;". % small i, acute accent
code_html(0'??) --> "&icirc;". % small i, circumflex accent
code_html(0'??) --> "&iuml;". % small i, umlaut mark
code_html(0'??) --> "&eth;". % small eth, Icelandic
code_html(0'??) --> "&ntilde;". % small n, tilde
code_html(0'??) --> "&ograve;". % small o, grave accent
code_html(0'??) --> "&oacute;". % small o, acute accent
code_html(0'??) --> "&ocirc;". % small o, circumflex accent
code_html(0'??) --> "&otilde;". % small o, tilde
code_html(0'??) --> "&ouml;". % small o, umlaut mark
code_html(0'??) --> "&oslash;". % small o, slash
code_html(0'??) --> "&ugrave;". % small u, grave accent
code_html(0'??) --> "&uacute;". % small u, acute accent
code_html(0'??) --> "&ucirc;". % small u, circumflex accent
code_html(0'??) --> "&uuml;". % small u, umlaut mark
code_html(0'??) --> "&yacute;". % small y, acute accent
code_html(0'??) --> "&thorn;". % small thorn, Icelandic
code_html(0'??) --> "&yuml;". % small y, umlaut mark
