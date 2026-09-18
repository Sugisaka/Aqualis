//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

    open System

    /// One CSS property and its serialized value.
    type CSS = {Key:string; Value:string}

    /// Validates and escapes HTML names and values.
    [<RequireQualifiedAccess>]
    module internal HtmlEncoding =
        /// Escapes text for insertion into HTML content.
        let textContent = HtmlTextEncoding.textContent

        /// Escapes text for insertion into an HTML attribute.
        let attributeValue (value:string) =
            if isNull value then nullArg (nameof value)

            value
                .Replace("&", "&amp;")
                .Replace("\"", "&quot;")
                .Replace("'", "&#39;")
                .Replace("<", "&lt;")
                .Replace(">", "&gt;")

        /// Validates an HTML attribute name.
        let attributeName (name:string) =
            if String.IsNullOrWhiteSpace name then
                invalidArg (nameof name) "An HTML attribute name is required."

            let validFirst character =
                Char.IsLetter character || character = '_' || character = ':'
            let validRest character =
                validFirst character || Char.IsDigit character || character = '-' || character = '.'

            if not (validFirst name[0]) || name |> Seq.skip 1 |> Seq.exists (validRest >> not) then
                invalidArg (nameof name) "The HTML attribute name contains invalid characters."

            name

        /// Validates an HTML element name.
        let elementName (name:string) =
            if String.IsNullOrWhiteSpace name then
                invalidArg (nameof name) "An HTML element name is required."

            let valid character =
                Char.IsLetterOrDigit character || character = '-' || character = ':'

            if not (Char.IsLetter name[0]) || name |> Seq.exists (valid >> not) then
                invalidArg (nameof name) "The HTML element name contains invalid characters."

            name

    /// HTML attribute with an optional value and escaped serialization.
    type Atr private (s:string,t:string option) =
        new(s:string,t:string) = Atr(s,Some t)
        new(s:string) = Atr(s,None)
        // new(s:Style) =
        //     let h:string = s.code
        //     Atr h
        /// Gets the attribute name.
        member _.name with get() = s
        /// Gets the attribute value, or an empty string for a valueless attribute.
        member _.value with get() = defaultArg t ""
        /// Gets serialized markup for the attribute or expression.
        member _.code with get() =
            let name = HtmlEncoding.attributeName s
            match t with
            | None -> name
            | Some value -> name + "=\"" + HtmlEncoding.attributeValue value + "\""
        /// Renders a list of HTML attributes.
        static member list(s:list<Atr>) =
            String.concat " " (
                s
                |> List.map (fun (s:Atr) -> s.code)
                |> List.filter (fun s -> s<>""))

    /// Ordered collection of CSS declarations for an inline style attribute.
    and Style(s:list<CSS>) =
        /// Gets the underlying collection of attributes or style properties.
        member _.list with get() = s
        /// Gets the serialized inline CSS declarations.
        member _.code0 with get() =
            s
            |> List.map (fun s -> s.Key+": "+s.Value)
            |> fun s -> String.concat "; " s
        // member _.code with get() =
        //     s
        //     |> List.map (fun s -> s.Key+": "+s.Value)
        //     |> fun s -> String.concat "; " s
        //     |> fun s -> if s = "" then "" else "style = \""+s+"\""
        /// Gets this style as an HTML style attribute.
        member this.atr with get() = Atr("style", this.code0)
        /// Combines CSS declaration lists in order.
        static member (+) (a:Style,b:Style) = Style(a.list@b.list)
        /// Gets an empty CSS style.
        static member blank = Style []

    /// Formats numeric CSS lengths using invariant culture.
    [<RequireQualifiedAccess>]
    module CssLength =
        /// Formats a floating-point value as a CSS pixel length.
        let pixels (value:float) =
            InvariantFormat.number value + "px"

        /// Formats an integer as a CSS pixel length.
        let pixelsInt (value:int) =
            InvariantFormat.integer value + "px"

    /// CSS style construction helpers.
    [<AutoOpen>]
    module style =
        /// Creates a CSS declaration for z-index.
        let zindex(n:int) = {Key="z-index"; Value=InvariantFormat.integer n}
        /// CSS area helpers.
        module area =
            /// Creates a CSS declaration for background-color.
            let backGroundColor (s:string) = {Key="background-color"; Value=s}
            /// Creates a CSS declaration for background-size.
            let backGroundSize (s:string) = {Key="background-size"; Value=s}
            /// Creates a CSS declaration for background-image.
            let backGroundImage (filename:string) = {Key="background-image"; Value="url("+filename+")"}
            /// Creates a CSS declaration for background-opacity.
            let opacity (s:string) = {Key="background-opacity"; Value=s}
        /// CSS font helpers.
        module font =
            /// Creates a CSS declaration for font-size.
            let size (s:int) = {Key="font-size"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for color.
            let color (s:string) = {Key="color"; Value=s}
            /// Creates a CSS declaration for font-weight.
            let weight (s:string) = {Key="font-weight"; Value=s.ToString()}
            /// Creates a CSS declaration for font-family.
            let family (s:string) = {Key="font-family"; Value=s}
            /// Creates a CSS declaration for line-height.
            let lineHeight (s:int) = {Key="line-height"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for font-style.
            let style (s:string) = {Key="font-style"; Value=s}
        /// CSS size helpers.
        module size =
            /// Creates a CSS declaration for width.
            let width (s:string) = {Key="width"; Value=s}
            /// Creates a CSS declaration for height.
            let height (s:string) = {Key="height"; Value=s}
            /// Creates a CSS declaration for max-width.
            let maxWidth (s:string) = { Key = "max-width"; Value = s }
        /// CSS margin helpers.
        module margin =
            /// Creates a CSS declaration for margin-left.
            let left (s:string) = {Key="margin-left"; Value=s}
            /// Creates a CSS declaration for margin-right.
            let right (s:string) = {Key="margin-right"; Value=s}
            /// Creates a CSS declaration for margin-top.
            let top (s:string) = {Key="margin-top"; Value=s}
            /// Creates a CSS declaration for margin-bottom.
            let bottom (s:string) = {Key="margin-bottom"; Value=s}
            /// Creates a CSS declaration for margin-left.
            let leftPx (s:float) = {Key="margin-left"; Value=CssLength.pixels s}
            /// Creates a CSS declaration for margin-right.
            let rightPx (s:float) = {Key="margin-right"; Value=CssLength.pixels s}
            /// Creates a CSS declaration for margin-top.
            let topPx (s:float) = {Key="margin-top"; Value=CssLength.pixels s}
            /// Creates a CSS declaration for margin-bottom.
            let bottomPx (s:float) = {Key="margin-bottom"; Value=CssLength.pixels s}
            /// Creates a CSS declaration for margin.
            let all (s:int) = {Key="margin"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for margin.
            let custom (s:string) = {Key="margin"; Value=s}
        /// CSS padding helpers.
        module padding =
            /// Creates a CSS declaration for padding-left.
            let left (s:int) = {Key="padding-left"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for padding-right.
            let right (s:int) = {Key="padding-right"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for padding-top.
            let top (s:int) = {Key="padding-top"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for padding-bottom.
            let bottom (s:int) = {Key="padding-bottom"; Value=CssLength.pixelsInt s}
            /// Creates a CSS declaration for padding.
            let all (s:int) = {Key="padding"; Value=CssLength.pixelsInt s}
            /// Creates CSS padding with vertical and horizontal pixel values.
            let paddingVH (v:int,h:int) =
                {Key="padding"; Value=CssLength.pixelsInt v + " " + CssLength.pixelsInt h}
        /// CSS border helpers.
        module border =
            /// Creates a CSS declaration for border.
            let style (s:string) = {Key="border"; Value=s}
            /// Creates a CSS declaration for border-color.
            let color (s:string) = {Key="border-color"; Value=s}
            /// CSS width helpers.
            module width =
                /// Creates a CSS declaration for border-top-width.
                let top (s:int) = {Key="border-top-width"; Value=CssLength.pixelsInt s}
                /// Creates a CSS declaration for border-bottom-width.
                let bottom (s:int) = {Key="border-bottom-width"; Value=CssLength.pixelsInt s}
                /// Creates a CSS declaration for border-left-width.
                let left (s:int) = {Key="border-left-width"; Value=CssLength.pixelsInt s}
                /// Creates a CSS declaration for border-right-width.
                let right (s:int) = {Key="border-right-width"; Value=CssLength.pixelsInt s}
        /// CSS stroke helpers.
        module stroke =
            /// Creates a CSS declaration for stroke.
            let color (s:string) = {Key="stroke"; Value=s}
            /// Creates a CSS declaration for stroke-width.
            let width (s:float) = {Key="stroke-width"; Value=CssLength.pixels s}
            /// Creates a CSS declaration for stroke-dasharray.
            let dasharray (s:list<int>) = {Key="stroke-dasharray"; Value=String.Join(" ",s |> List.map InvariantFormat.integer)}
            /// Creates a CSS declaration for stroke-opacity.
            let opacity(s:float) = {Key="stroke-opacity"; Value=InvariantFormat.number s}
        /// CSS fill helpers.
        module fill =
            /// Creates a CSS declaration for fill.
            let color (s:string) = {Key="fill"; Value=s}
            /// Creates a CSS declaration for fill-opacity.
            let opacity(s:float) = {Key="fill-opacity"; Value=InvariantFormat.number s}
        /// CSS align helpers.
        module align =
            /// CSS items helpers.
            module items =
                /// Creates a CSS declaration for align-items.
                let center = {Key="align-items"; Value="center"}
            /// Creates a CSS declaration for justify-content.
            let justifyContent (s:string) = {Key="justify-content"; Value=s}
            /// Creates a CSS declaration for text-align.
            let text (s:string) = {Key="text-align"; Value=s}
            /// Creates a CSS declaration for vertical-align.
            let vertical (s:string) = {Key="vertical-align"; Value=s}
            /// Creates a CSS declaration for text-decoration.
            let textDecoration (s:string) = {Key = "text-decoration"; Value = s}
            /// Creates a CSS declaration for float.
            let float (s:string) = {Key = "float"; Value = s}
        /// CSS display helpers.
        module display =
            /// Creates a CSS declaration for display.
            let flex = {Key="display"; Value="flex"}
            /// Creates a CSS declaration for display.
            let display (s:string) = {Key="display"; Value= s}
            /// Creates a CSS declaration for gap.
            let gap (s:string) = {Key="gap"; Value=s}
            /// Creates a CSS declaration for visibility.
            let visibility (s:string) = {Key="visibility"; Value= s}
        /// CSS list helpers.
        module list =
            /// Creates a CSS declaration for list-style.
            let listStyle (s:string) = {Key="list-style"; Value=s}
        /// CSS bidi helpers.
        module bidi =
            /// Creates a CSS declaration for unicode-bidi.
            let unicodeBidi (s:string) = {Key="unicode-bidi"; Value=s}
        /// CSS overflow helpers.
        module overflow =
            /// Creates a CSS declaration for overflow-clip-margin.
            let clipMargin (s:string) = {Key = "overflow-clip-margin"; Value = s}
            /// Creates a CSS declaration for overflow.
            let overflow (s:string) = {Key = "overflow"; Value = s}
        /// CSS cursor helpers.
        module cursor =
            /// Creates a CSS declaration for cursor.
            let custom (s:string) = { Key = "cursor"; Value = s }
        /// CSS objectFit helpers.
        module objectFit =
            /// Creates a CSS declaration for object-fit.
            let custom (s:string) = {Key = "object-fit"; Value = s}
        /// CSS flex helpers.
        module flex =
            /// Creates a CSS declaration for flex-wrap.
            let wrap (s:string) = {Key="flex-wrap"; Value=s}
        /// CSS position helpers.
        module position =
            /// Creates a CSS declaration for position.
            let position (s:string) = {Key="position"; Value=s}
            /// Creates a CSS declaration for z-index.
            let index (s:int) = {Key="z-index"; Value=InvariantFormat.integer s}
        /// CSS space helpers.
        module space =
            /// Creates a CSS declaration for white-space.
            let space (s:string) = {Key = "white-space"; Value = s.ToString();}

    /// Four edge values describing a layout anchor.
    type Anchor = {Left:double; Right:double; Top:double; Bottom:double;}

    /// Two-dimensional position used by HTML and figure layout helpers.
    type position(xx:float,yy:float) =
        new(ix:int,iy:int) =
            position(float ix,float iy)
        /// Gets the horizontal coordinate.
        member this.x with get() = xx
        /// Gets the vertical coordinate.
        member this.y with get() = yy
        /// Returns a position translated by the supplied offsets.
        member this.shift(x,y) = position(xx+x,yy+y)
        /// Returns a position translated horizontally.
        member this.shiftX(x) = this.shift(x,0)
        /// Returns a position translated vertically.
        member this.shiftY(y) = this.shift(0,y)
        /// Returns a copy of this position.
        member this.origin = this.shift(0,0)
        /// Gets the coordinate origin.
        static member Origin with get() = position(0,0)
        /// Adds the coordinates of two positions.
        static member (+) (p1:position,p2:position) = position(p1.x+p2.x, p1.y+p2.y)
        /// Subtracts the coordinates of one position from another.
        static member (-) (p1:position,p2:position) = position(p1.x-p2.x, p1.y-p2.y)

    /// Configures optional MathJax and font assets for generated HTML.
    type HtmlAssetSettings internal (context:Aqualis) =
        let ensureHtml() =
            match context.language with
            | HTML | HTMLSequenceDiagram | PHP -> ()
            | _ ->
                raise (NotSupportedException(
                    "HTML assets are only supported by HTML, HTML sequence diagram, and PHP generation contexts."))

        /// <summary>Uses the specified local or HTTPS MathJax script. No script is configured by default.</summary>
        member _.UseMathJax(scriptUrl:Url) =
            ensureHtml()
            context.htmlAssets.SetMathJaxScript(Url.value scriptUrl)

        /// <summary>Stops emitting a MathJax script reference.</summary>
        member _.DisableMathJax() =
            ensureHtml()
            context.htmlAssets.DisableMathJax()

        /// <summary>Uses the specified local or HTTPS font stylesheet. System fonts are used by default.</summary>
        member _.UseFontStylesheet(stylesheetUrl:Url) =
            ensureHtml()
            context.htmlAssets.SetFontStylesheet(Url.value stylesheetUrl)

        /// <summary>Stops emitting a font stylesheet reference.</summary>
        member _.UseSystemFonts() =
            ensureHtml()
            context.htmlAssets.UseSystemFonts()

    /// CSS HtmlAssetSettingsExtensions helpers.
    [<AutoOpen>]
    module HtmlAssetSettingsExtensions =
        type Aqualis with
            /// <summary>Configures optional assets referenced by generated HTML.</summary>
            member this.HtmlAssets = HtmlAssetSettings this

    /// Emits configured HTML scripts and stylesheets.
    [<RequireQualifiedAccess>]
    module internal HtmlAssetRendering =
        /// Writes configured scripts and stylesheets into the HTML output.
        let write indent (writeLine:string -> unit) (assets:HtmlAssetController) =
            let mathJaxScript,fontStylesheet = assets.Snapshot
            match mathJaxScript with
            | Some scriptUrl ->
                writeLine (indent + "<script>MathJax = { chtml: { displayAlign: \"left\" } };</script>")
                writeLine (
                    indent +
                    "<script type=\"text/javascript\" id=\"MathJax-script\" async src=\"" +
                    HtmlEncoding.attributeValue scriptUrl +
                    "\"></script>")
            | None -> ()
            match fontStylesheet with
            | Some stylesheetUrl ->
                writeLine (
                    indent +
                    "<link rel=\"stylesheet\" href=\"" +
                    HtmlEncoding.attributeValue stylesheetUrl +
                    "\">")
            | None -> ()

    /// Writes HTML elements and positioned content to a generation context.
    type html internal (c:Aqualis) =
        /// Writes raw HTML to the generation context.
        let write(s:string) = c.codewrite s
        let writei(s:string) = c.codewritei s
        let writen(s:string) = c.codewriten s
        let writein(s:string) = c.codewritein s
        let writeConfiguredAssets() = HtmlAssetRendering.write "    " writein c.htmlAssets
        /// Gets the associated generation context.
        member _.Context with get() = c
        /// Writes a value as an HTML text node.
        member _.text(value:string) = writein(HtmlEncoding.textContent value)
        /// Writes the document head and invokes the content callback.
        member this.head title = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<head>"
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta http-equiv=\"content-language\" content=\"ja\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein("    <title>" + HtmlEncoding.textContent title + "</title>")
            writeConfiguredAssets()
            writein "    <link rel='stylesheet' href='style.css' />"
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        /// Writes the document head and invokes the content callback.
        member this.head (title,refresh:int) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<head>"
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta http-equiv=\"content-language\" content=\"ja\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein("    <title>" + HtmlEncoding.textContent title + "</title>")
            writeConfiguredAssets()
            writein "    <link rel='stylesheet' href='style.css' />"
            writein("    <meta http-equiv=\"refresh\" content=\""+InvariantFormat.integer refresh+"\">")
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        /// Writes the document head and invokes the content callback.
        member this.head (title:string,cssfile:Url,jsfile:Url,refresh:int) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<head>"
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta http-equiv=\"content-language\" content=\"ja\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein("    <title>" + HtmlEncoding.textContent title + "</title>")
            writeConfiguredAssets()
            this.taga ("link", [Atr("rel", "stylesheet"); Atr("href", Url.value cssfile)])
            this.tagb ("script", [Atr("type", "text/javascript"); Atr("src", Url.value jsfile)]) ignore
            writein("    <meta http-equiv=\"refresh\" content=\""+InvariantFormat.integer refresh+"\">")
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        /// Writes the document head and invokes the content callback.
        member this.head (title:string,cssfile:Url,jsfile:Url) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<head>"
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta http-equiv=\"content-language\" content=\"ja\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein("    <title>" + HtmlEncoding.textContent title + "</title>")
            writeConfiguredAssets()
            this.taga ("link", [Atr("rel", "stylesheet"); Atr("href", Url.value cssfile)])
            this.tagb ("script", [Atr("type", "text/javascript"); Atr("src", Url.value jsfile)]) ignore
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        /// Writes the document head and invokes the content callback.
        member this.head (title:string,cssfile:Url) = fun code ->
            writein "<!doctype html>"
            writein "<html lang=\"ja\">"
            writein "<head>"
            writein "    <meta charset=\"utf-8\">"
            writein "    <meta http-equiv=\"content-language\" content=\"ja\">"
            writein "    <meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0'>"
            writein("    <title>" + HtmlEncoding.textContent title + "</title>")
            writeConfiguredAssets()
            this.taga ("link", [Atr("rel", "stylesheet"); Atr("href", Url.value cssfile)])
            writein "</head>"
            writein "<body>"
            code()
            writein "</body>"
            writein "</html>"
        // /// 内部要素のないタグ
        // member this.taga (t:string,s:Style) =
        //     writein("<"+t+" "+s.code+" />")
        /// 内部要素のないタグ
        member this.taga (t:string,atr:list<Atr>) =
            let tag = HtmlEncoding.elementName t
            writein("<"+tag+" "+Atr.list atr+" />")
        // 内部要素のないタグ
        // member this.taga (t:string,lst:list<string*string>) =
        //     writein("<"+t+" ")
        //     for a,s in lst do
        //         writein(a + "=" + s + " ")
        //     writein " />"
        /// 内部要素のないタグ
        member this.taga (t:string) =
            let tag = HtmlEncoding.elementName t
            writein("<"+tag+" ")
            writein " />"
        // /// 内部要素のあるタグ
        // member this.tagb (t:string,atr:Style) = fun code ->
        //     let a = atr.code
        //     if a = "" then
        //         writein("<"+t+">")
        //     else
        //         writein("<"+t+" "+a+" >")
        //     code()
        //     writein ("</"+t+">")
        /// 内部要素のあるタグ
        member this.tagb (t:string,atr:list<Atr>) = fun code ->
            let tag = HtmlEncoding.elementName t
            let a = Atr.list atr
            if a = "" then
                writein("<"+tag+">")
            else
                writein("<"+tag+" "+a+" >")
            code()
            writein ("</"+tag+">")

        // /// 内部要素のあるタグ
        // member this.tagb (t:string,lst:list<string*string>) = fun code ->
        //     if lst.Length=0 then
        //         writein("<"+t+">")
        //     else
        //         writein("<"+t+" ")
        //         for a,s in lst do
        //             writein(a + "=\"" + s + "\" ")
        //         writein ">"
        //     code()
        //     writein ("</"+t+">")
        /// 内部要素のあるタグ
        member this.tagb (t:string) = fun code ->
            let tag = HtmlEncoding.elementName t
            writein("<"+tag+">")
            code()
            writein ("</"+tag+">")

        // /// 内部要素のあるタグ
        // member this.tagb0 (t:string,lst:list<string*string>) = fun code ->
        //     if lst.Length=0 then
        //         write("<"+t+">")
        //     else
        //         writen("<"+t+" ")
        //         for a,s in lst do
        //             writen(a + " = \"" + s + "\"")
        //         write ">"
        //     code()
        //     writen ("</"+t+">")

        /// Writes an opening HTML tag.
        member this.tagv (t:string,atr:list<Atr>) =
            writein("<" + HtmlEncoding.elementName t + " " + Atr.list atr + ">")

        /// Writes a closing HTML tag.
        member this.tage (t:string) =
            writein("</" + HtmlEncoding.elementName t + ">")

        /// Writes a level-one heading and its nested content.
        member this.h1 (t:string) = fun code ->
            this.tagb "h1" <| fun () -> this.text t
            code()

        /// Writes a level-one heading and its nested content.
        member this.h1 (t:string,s:Style) = fun code ->
            this.tagb ("h1",[s.atr]) <| fun () -> this.text t
            code()

        /// Writes a level-two heading and its nested content.
        member this.h2 (t:string) = fun code ->
            this.tagb "h2" <| fun () -> this.text t
            code()

        /// Writes a level-two heading and its nested content.
        member this.h2 (t:string,s:Style) = fun code ->
            this.tagb ("h2",[s.atr]) <| fun () -> this.text t
            code()
        /// Writes a level-three heading and its nested content.
        member this.h3 (t:string) = fun code ->
            this.tagb "h3" <| fun () -> this.text t
            code()
        /// Writes a level-three heading and its nested content.
        member this.h3 (t:string,s:Style) = fun code ->
            this.tagb ("h3",[s.atr]) <| fun () -> this.text t
            code()
        /// Writes a level-four heading and its nested content.
        member this.h4 (t:string) = fun code ->
            this.tagb "h4" <| fun () -> this.text t
            code()
        /// Writes a level-four heading and its nested content.
        member this.h4 (t:string,s:Style) = fun code ->
            this.tagb ("h4",[s.atr]) <| fun () -> this.text t
            code()
        /// Writes a level-five heading and its nested content.
        member this.h5 (t:string) = fun code ->
            this.tagb "h5" <| fun () -> this.text t
            code()
        /// Writes a level-five heading and its nested content.
        member this.h5 (t:string,s:Style) = fun code ->
            this.tagb ("h5",[s.atr]) <| fun () -> this.text t
            code()
        /// Writes a POST form directed to the validated URL.
        member this.form (action:Url) = fun code -> this.tagb ("form",[Atr("method","post"); Atr("action",Url.value action);]) code
        /// Writes a multipart POST form for file uploads.
        member this.form_fileUpload (action:Url) = fun code -> this.tagb ("form",[Atr("method","post"); Atr("enctype","multipart/form-data"); Atr("action",Url.value action);]) code
        /// Writes a submit control with a validated form action.
        member this.submit(url:Url,name:string,value:string) = this.taga("input",[Atr("type","submit"); Atr("name",name); Atr("value",value); Atr("formaction",Url.value url)])
        // member this.table_ code = this.tagb "table" code
        /// Writes a table around the content callback.
        member this.table (a:list<Atr>) = fun code -> this.tagb ("table",a) code
        /// Writes rows and cells for a string table.
        member this.tableData (lst:list<list<string>>) = fun (p:position) (size:int) ->
            writein ("<table style =\"margin-left: "+InvariantFormat.number p.x+"px; margin-top: "+InvariantFormat.number p.y+"px; font-size: "+InvariantFormat.integer size+"px; position: absolute;\">")
            for m in 0..lst.Length-1 do
                writein "<tr>"
                for s in lst[m] do
                    writein "<td>"
                    this.text s
                    writein "</td>"
                writein "</tr>"
            writein "</table>"
            writein "</div>"
        // member this.tr code = this.tagb "tr" code
        /// Writes a table row around the content callback.
        member this.tr (a:list<Atr>) = fun code -> this.tagb ("tr",a) code
        /// Writes a table header cell around the content callback.
        member this.th (a:list<Atr>) code = this.tagb ("th",a) code
        /// Writes a table data cell around the content callback.
        member this.td (a:list<Atr>) code = this.tagb ("td",a) code
        // member this.td (a:list<string*string>) = fun code -> this.tagb ("td",a) code
        /// Writes the supplied text inside a strong element.
        member this.strong(t:string) = this.tagb "strong" <| fun () -> this.text t
        // member this.enumerate code = this.tagb "ol" code
        /// Writes an ordered list around the content callback.
        member this.enumerate (a:list<Atr>) = fun code -> this.tagb ("ol",a) code
        // member this.enumerate (a:Style) = fun code -> this.tagb ("ol",a) code
        /// Writes an ordered list from item callbacks.
        member this.enumerateList (a:list<Atr>) (c:list<unit->unit>) =
            this.tagb "ol" <| fun () ->
                for x in c do
                    this.item a x
        /// Writes an unordered list around the content callback.
        member this.itemize code = this.tagb "ul" code
        /// Writes an unordered list around the content callback.
        member this.itemize (a:list<Atr>) = fun code -> this.tagb ("ul",a) code
        // member this.itemize (a:Style) = fun code -> this.tagb ("ul",a) code
        /// Writes an unordered list from item callbacks.
        member this.itemizeList (a:list<Atr>) (c:list<unit->unit>) =
            this.tagb "ul" <| fun () ->
                for x in c do
                    this.item a x
        // member this.item code = this.tagb "li" code
        // member this.item (a:Style) = fun code -> this.tagb ("li",a) code
        /// Writes a list item around the content callback.
        member this.item (a:list<Atr>) = fun code -> this.tagb ("li",a) code
        /// Writes a paragraph from text or a content callback.
        member this.para code = this.tagb "p" code
        /// Writes a paragraph from text or a content callback.
        member this.para (a:list<Atr>) = this.tagb ("p",a)
        /// Writes a paragraph from text or a content callback.
        member this.para (t:string) = this.tagb "p" <| fun () -> this.text t
        /// Writes a span with the supplied class and content.
        member this.span(cls:string,t:string) = this.tagb ("span",[Atr("class",cls)]) <| fun () -> this.text t
        /// Writes a span with the supplied class and content.
        member this.span(cls:string) = fun code -> this.tagb ("span",[Atr("class",cls)]) code
        /// Writes a span with the supplied class and content.
        member this.span(cls:string, s:Style) = fun code -> this.tagb ("span",[s.atr; Atr("class",cls)]) code
        /// Writes an anchor to a validated URL.
        member this.link(url:Url) = fun code -> this.tagb ("a",[Atr("href",Url.value url);]) code
        /// Writes an anchor to a validated URL.
        member this.link(url:Url, s:Style) = fun code -> this.tagb ("a",[s.atr; Atr("href",Url.value url)]) code
        /// Writes an anchor that opens a validated URL in a new tab.
        member this.link_newtab(url:Url) = fun code -> this.tagb ("a",[Atr("href",Url.value url); Atr("target","_blank"); Atr("rel","noopener noreferrer")]) code
        /// Writes a disabled select element.
        member this.select_disabled(x:string) = fun code -> this.tagb ("select",[Atr("name",x); Atr("disabled","disabled")]) code
        /// Writes a time element with the supplied style.
        member this.time(datatime:string, s:Style) = fun code -> this.tagb ("time",[s.atr; Atr("datatime",datatime)]) code
        /// Writes an article element with the supplied class.
        member this.article(cls:string) = fun code -> this.tagb ("article", [Atr("class", cls)]) code
        /// Writes an aside element around the content callback.
        member this.aside (cls:string, s:Style) = fun code -> this.tagb ("aside", [s.atr; Atr("class", cls)]) code
        /// Writes an aside element around the content callback.
        member this.aside (a:list<Atr>) = fun code -> this.tagb ("aside",a) code
        /// Writes a section element around the content callback.
        member this.section(cls:string, s:Style) = fun code -> this.tagb ("section", [s.atr; Atr("class", cls)]) code
        /// Writes an option element with the supplied value.
        member this.option(value:string) = fun code -> this.tagb ("option",[Atr("value",value);]) code
        /// Writes a selected option element.
        member this.option_selected(value:string) = fun code -> this.tagb ("option",[Atr("value",value);Atr("selected","selected");]) code
        // member this.div (a:list<Atr>) = fun code -> this.tagb ("div",a) code
        /// Writes a button input with an onclick handler.
        member this.button(value:string,onclick:string) = this.taga("input",[Atr("type","button"); Atr("value",value); Atr("onclick",onclick);])
        /// Writes a bold element around the content callback.
        member this.bold code = this.tagb "b" code
        /// Writes a LaTeX-style tag around the content callback.
        member this.latexTag (tagname:string) code =
            writein("\\begin{"+tagname+"}")
            code()
            writein("\\end{"+tagname+"}")
        /// Wraps an expression in inline math delimiters.
        member this.eq (q:string) = "\\("+q+"\\)"
        /// Writes aligned mathematical content.
        member this.align code =
            writein "\\[\\begin{align}"
            code()
            writein "\\end{align}\\]"
        /// Writes a footer around the content callback.
        member this.footer code = this.tagb ("footer", [Atr("class","footer")]) <| fun () -> code()
        /// Writes a footer around the content callback.
        member this.footer (s:Style) = fun code -> this.tagb ("footer", [s.atr]) <| fun () -> code()
        /// Writes a line-break element.
        member this.br() = writein "<br>"
        /// Writes a horizontal-rule element.
        member this.hr() = writein "<hr>"
        /// Adds a JavaScript file to the generated page.
        member this.setjs (filename:Url) =
            this.tagb ("script",[Atr("src",Url.value filename)]) <| fun () -> ()
        /// Writes positioned title text with the supplied style.
        member this.title (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "font-weight"; Value = "bold";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "90px";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                this.text text
        /// Writes positioned body text with the supplied style.
        member this.contents (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "40px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "25px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                this.text text
        /// Writes positioned subtitle text.
        member this.subtitle1 (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "40px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "15px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                this.text text
        /// Writes positioned subtitle text.
        member this.subtitle2 (s:Style) (p:position) (text:string) =
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-family"; Value = "'Noto Sans JP'";}
                            {Key = "color"; Value = "black";}
                            {Key = "white-space"; Value = "nowrap";}
                            {Key = "font-size"; Value = "30px";}
                            {Key = "border-left-style"; Value= "solid";}
                            {Key = "border-left-width"; Value= "15px";}
                            {Key = "border-left-color"; Value= "#1e6eff";}
                            {Key = "border-bottom-style"; Value= "solid";}
                            {Key = "border-bottom-width"; Value= "2px";}
                            {Key = "border-bottom-color"; Value= "#1e6eff";}
                            {Key = "padding-left"; Value="10px";}
                            {Key = "display"; Value="inline-block";}]
            this.tagb ("div",[(s1+s).atr]) <| fun () ->
                this.text text
        /// Writes a div element with the supplied style or attributes.
        member this.div (s:Style) = fun (p:position) code ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            this.tagb ("div", [(s1+s).atr]) code
        /// Writes escaped text to the generated HTML.
        member this.text (s:Style) = fun (p:position) (text:string) ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                this.text text

        /// Writes a styled div around the content callback.
        member this.canvas (s:Style) code =
            this.tagb ("div", [s.atr]) <| fun () ->
                code ()

        /// Writes a div element with the supplied style or attributes.
        member this.div (cls:string, s:Style) = fun code ->
            this.tagb ("div", [s.atr; Atr("class", cls)]) code

        /// Writes a div element with the supplied style or attributes.
        member this.div (s:list<Atr>) = fun code ->
            this.tagb ("div", s) code

        /// Writes positioned figure content.
        member this.fig (p:position) code =
            let f = figure(this.taga)
            code(f,p)
            let sx,sy,mx,my = f.setWriteMode()
            writein (
                "<svg viewBox=\"0 0 "+InvariantFormat.number sx+" "+InvariantFormat.number sy+"\" "+
                "width=\""+CssLength.pixels sx+"\" "+
                "height=\""+CssLength.pixels sy+"\" "+
                "xmlns=\"http://www.w3.org/2000/svg\" "+
                "style=\"margin-left: "+CssLength.pixels mx+"; "+
                "margin-top: "+CssLength.pixels my+"; "+
                "position: absolute;"+
                "\">")
            code(f,p)
            writein "</svg>"

        /// Writes a styled block of text at a position.
        member this.blockTextcode (s:Style) (p:position) (width:float,height:float) (borderWidth:float,borderStyle:string,borderColor:string) (text:list<string>) =
            let padding = 5
            let s1 = Style [size.width (InvariantFormat.number width+"px")
                            size.height (InvariantFormat.number height+"px")
                            font.family "'Noto Sans Mono',monospace"
                            {Key = "margin-left"; Value = InvariantFormat.number p.x + "px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y + "px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "overflow-wrap"; Value = "break-word";}
                            {Key = "border-width"; Value = InvariantFormat.number borderWidth + "px";}
                            {Key = "border-style"; Value = borderStyle;}
                            {Key = "border-color"; Value = borderColor;}]
            this.tagb ("div", [(s1+s).atr])
                <| fun () ->
                    text |> List.iter (fun s ->
                        this.text s
                        writein "<br>")
                    writein ""
            {Left = p.x;
            Right = p.x+double width+2.0*double padding+2.0*double borderWidth;
            Top = p.y;
            Bottom = p.y+double height+2.0*double padding+2.0*double borderWidth;}

        /// Writes text inside a positioned frame.
        member this.textFrame (s:Style) = fun (p:position) (size:int) (color:string) code ->
            let s1 = Style [{Key = "margin-left"; Value = InvariantFormat.number p.x+"px";}
                            {Key = "margin-top"; Value = InvariantFormat.number p.y+"px";}
                            {Key = "position"; Value = "absolute";}
                            {Key = "font-size"; Value = CssLength.pixelsInt size;}
                            {Key = "color"; Value = color.ToString();}]
            this.tagb ("div", [(s1+s).atr]) <| fun () ->
                code()

        /// Writes an equation inside a positioned frame.
        member this.equationFrame (s:Style) = fun (p:position) (size:int) (color:string) code ->
            this.textFrame s p size color <| fun () ->
                writein "\\("
                code()
                writein "\\)"

        /// Writes aligned content inside a positioned frame.
        member this.alignFrame (s:Style) = fun (p:position) (size:int) (color:string) code ->
            this.textFrame s p size color <| fun () ->
                writein "\\["
                writein "\\begin{align}"
                code()
                writein "\\end{align}"
                writein "\\]"

        /// Writes a line or polyline through the supplied positions.
        member this.line (s:Style) (pp:list<position>) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.polyLine s pp
        /// Writes a polyline through the supplied positions.
        member this.polyLine (s:Style) (pp:list<position>) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.polyLine s pp
        /// Writes an arrow along the supplied positions.
        member this.arrow (lineStyle:Style,arrowStyle:Style,width,arrowsize) (pp:list<position>) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.polyLineTriangleArrow (lineStyle,arrowStyle,width,arrowsize) pp
        /// Writes a circle with the supplied center and radius.
        member this.circle (s:Style) (ps:position) (r:int) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.ellipse s (p + ps) r r
        /// Writes a rectangle with the supplied position and size.
        member this.rectangle (s:Style) (ps:position) (w:int,h:int) =
            this.fig (position(0.0,0.0)) <| fun (f,p) ->
                f.rect s (p + ps) w h

        /// Creates a graph coordinate region and invokes the drawing callback.
        member this.graph (px0:double,py0:double) (sizeX:double,sizeY:double) (x1:double,x2:double) (y1:double,y2:double) code =
            this.fig (position(px0,py0)) <| fun (f,p) ->
                if x1*x2<0.0 then
                    let x0 = (0.0-x1)/(x2-x1)*sizeX
                    f.triangleArrow Style[stroke.color "#000000"; fill.color "#000000";] (3.0,20) (p+position(x0,sizeY)) (p+position(x0,0.0))
                if y1*y2<0.0 then
                    let y0 = sizeY-(0.0-y1)/(y2-y1)*sizeY
                    f.triangleArrow Style[stroke.color "#000000"; fill.color "#000000";] (3.0,20) (p+position(0.0,y0)) (p+position(sizeX,y0))
                code(f,p)
        /// Plots one or more functions over a graph coordinate region.
        member this.graphEq (px0:double,py0:double) (sizeX:double,sizeY:double) (x1:double,x2:double,N:int) (y1:double,y2:double) (fn:list<Style*(double->double)>) =
            this.graph (px0,py0) (sizeX,sizeY) (x1,x2) (y1,y2) <| fun (f,p) ->
                for s,fc in fn do
                    let pol =
                        [
                            for i in 0..N do
                                let x = x1 + (x2-x1)*double i/double N
                                let y = fc x
                                let X = (x-x1)/(x2-x1)*sizeX
                                let Y = sizeY-(y-y1)/(y2-y1)*sizeY
                                p+position(X,Y)
                        ]
                    f.polyLine s pol
        /// Plots functions and invokes an additional drawing callback.
        member this.graphEqs (px0:double,py0:double) (sizeX:double,sizeY:double) (x1:double,x2:double,N:int) (y1:double,y2:double) (fn:list<Style*(double->double)>) code =
            let T (p:position) = position((p.x-x1)/(x2-x1)*sizeX, sizeY-(p.y-y1)/(y2-y1)*sizeY)
            this.graph (px0,py0) (sizeX,sizeY) (x1,x2) (y1,y2) <| fun (f,p) ->
                for s,fc in fn do
                    let pol =
                        [
                            for i in 0..N do
                                let x = x1 + (x2-x1)*double i/double N
                                let y = fc x
                                p + T (position(x,y))
                        ]
                    f.polyLine s pol
                let line (s:Style) (pp:list<position>) =
                    f.polyLine s (pp |> List.map (fun ps -> p + T ps))
                let arrow (lineStyle:Style,arrowStyle:Style,lineWidth,arrowSize) (pp:list<position>) =
                    f.polyLineTriangleArrow (lineStyle,arrowStyle,lineWidth,arrowSize) (pp |> List.map (fun ps -> p + T ps))
                let circle (s:Style) (ps:position) (r:int) =
                    f.ellipse s (p + T ps) r r
                let rectangle (s:Style) (ps:position) (w:float,h:float) =
                    let Ws = int <| w/(x2-x1)*sizeX
                    let Hs = int <| h/(y2-y1)*sizeY
                    f.rect s (p + T ps) Ws Hs
                let text (s:Style) (ps:position) text = ()
                code(line,arrow,circle,rectangle,text)
            let line (s:Style) (pp:list<position>) = ()
            let arrow (lineStyle:Style,arrowStyle:Style,lineWidth,arrowSize) (pp:list<position>) = ()
            let circle (s:Style) (ps:position) (r:int) = ()
            let rectangle (s:Style) (ps:position) (w:float,h:float) = ()
            let text (s:Style) (ps:position) text =
                this.text (Style[font.family "'Noto Sans JP', sans-serif";font.weight "600"; zindex 3]+s) (position(px0,py0) + T ps) text
            code(line,arrow,circle,rectangle,text)

    /// Collects figure bounds, then emits positioned SVG elements.
    and figure(writeTag:(string * list<Atr>) -> unit) =
        let padding = 10.0
        let mutable xmin:option<double> = None
        let mutable xmax:option<double> = None
        let mutable ymin:option<double> = None
        let mutable ymax:option<double> = None
        let mutable writeMode = false
        /// Gets the padding around the figure bounds.
        member _.Padding with get() = padding
        /// Gets the minimum recorded horizontal coordinate.
        member _.Xmin with get() = match xmin with |None -> 0.0 |Some v -> v
        /// Gets the maximum recorded horizontal coordinate.
        member _.Xmax with get() = match xmax with |None -> 0.0 |Some v -> v
        /// Gets the minimum recorded vertical coordinate.
        member _.Ymin with get() = match ymin with |None -> 0.0 |Some v -> v
        /// Gets the maximum recorded vertical coordinate.
        member _.Ymax with get() = match ymax with |None -> 0.0 |Some v -> v
        /// Switches the figure from bounds collection to markup emission.
        member this.setWriteMode() =
            writeMode <- true
            let sizeX = this.Xmax-this.Xmin+2.0*padding
            let sizeY = this.Ymax-this.Ymin+2.0*padding
            let marginX = this.Xmin-padding
            let marginY = this.Ymin-padding
            sizeX,sizeY,marginX,marginY

        /// Expands the tracked figure bounds to include a position.
        member private _.updateRange(p:position) =
            match xmin with
            |None ->
                xmin <- Some p.x
            |Some xx when p.x<xx ->
                xmin <- Some p.x
            |_ -> ()
            match ymin with
            |None ->
                ymin <- Some p.y
            |Some yy when p.y<yy ->
                ymin <- Some p.y
            |_ -> ()

            match xmax with
            |None ->
                xmax <- Some p.x
            |Some xx when p.x>xx ->
                xmax <- Some p.x
            |_ -> ()
            match ymax with
            |None ->
                ymax <- Some p.y
            |Some yy when p.y>yy ->
                ymax <- Some p.y
            |_ -> ()

        /// Records line bounds or emits a line element, depending on write mode.
        member this.line (s:Style) = fun (startP:position) (endP:position) ->
            if writeMode then
                writeTag ("line", [
                    Atr("x1",InvariantFormat.number (startP.x-this.Xmin+this.Padding));
                    Atr("y1",InvariantFormat.number (startP.y-this.Ymin+this.Padding));
                    Atr("x2",InvariantFormat.number (endP.x-this.Xmin+this.Padding));
                    Atr("y2",InvariantFormat.number (endP.y-this.Ymin+this.Padding));]@[s.atr])
            else
                this.updateRange startP
                this.updateRange endP

        /// Records line bounds or emits a line element with an ID, depending on write mode.
        member this.line (id:string) = fun (s:Style) (startP:position) (endP:position) ->
            if writeMode then
                writeTag ("line", [
                    Atr("id",id);
                    Atr("x1",InvariantFormat.number (startP.x-this.Xmin+this.Padding));
                    Atr("y1",InvariantFormat.number (startP.y-this.Ymin+this.Padding));
                    Atr("x2",InvariantFormat.number (endP.x-this.Xmin+this.Padding));
                    Atr("y2",InvariantFormat.number (endP.y-this.Ymin+this.Padding));]@[s.atr])
            else
                this.updateRange startP
                this.updateRange endP

        /// Records rectangle bounds or emits a rectangle, depending on write mode.
        member this.rect (s:Style) (startP:position) (sx:int) (sy:int) =
            if writeMode then
                writeTag ("rect", [
                    Atr("x", InvariantFormat.number (startP.x-this.Xmin+this.Padding));
                    Atr("y", InvariantFormat.number (startP.y-this.Ymin+this.Padding));
                    Atr("width",InvariantFormat.number sx)
                    Atr("height", InvariantFormat.number sy)]@[s.atr])
            else
                this.updateRange startP
                this.updateRange(startP.shift(sx,sy))

        /// Records ellipse bounds or emits an ellipse, depending on write mode.
        member this.ellipse (s:Style) (center:position) (radiusX:int) (radiusY:int) =
            if writeMode then
                writeTag ("ellipse", [
                    Atr("cx", InvariantFormat.number (center.x-this.Xmin+this.Padding));
                    Atr("cy", InvariantFormat.number (center.y-this.Ymin+this.Padding));
                    Atr("rx", InvariantFormat.number radiusX);
                    Atr("ry", InvariantFormat.number radiusY);]@[s.atr])
            else
                this.updateRange(center.shiftX -radiusX)
                this.updateRange(center.shiftX radiusX)
                this.updateRange(center.shiftY -radiusY)
                this.updateRange(center.shiftY radiusY)

        /// Records polygon bounds or emits a polygon, depending on write mode.
        member this.polygon (s:Style) (apex:list<position>) =
            if writeMode then
                let pp = String.concat " " <| List.map (fun (p:position) -> InvariantFormat.number (p.x-this.Xmin+this.Padding)+","+InvariantFormat.number (p.y-this.Ymin+this.Padding)) apex
                writeTag ("polygon", [Atr("points",pp)]@[s.atr])
            else
                for q in apex do
                    this.updateRange q

        /// Records polyline bounds or emits a polyline, depending on write mode.
        member this.polyLine (s:Style) (apex:list<position>) =
            if writeMode then
                let pp = String.concat " " <| List.map (fun (p:position) -> InvariantFormat.number (p.x-this.Xmin+this.Padding)+","+InvariantFormat.number (p.y-this.Ymin+this.Padding)) apex
                writeTag ("polyline", [Atr("points", pp)]@[s.atr])
            else
                for q in apex do
                    this.updateRange q

        /// Records bounds or emits a line with a triangular arrowhead.
        member this.triangleArrow (s:Style) (lineWidth:float,arrowSize:float) (startP:position) (endP:position) =
            let pi = 3.14159265358979
            let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
            let q1x = endP.x + arrowSize*cos(t0-15.0*pi/180.0)
            let q1y = endP.y + arrowSize*sin(t0-15.0*pi/180.0)
            let q2x = endP.x + arrowSize*cos(t0+15.0*pi/180.0)
            let q2y = endP.y + arrowSize*sin(t0+15.0*pi/180.0)
            let ux,uy =
                let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
                endP.x + (startP.x-endP.x)*c,
                endP.y + (startP.y-endP.y)*c
            if writeMode then
                this.line (s+Style[stroke.width lineWidth]) startP (position(ux,uy))
            else
                this.updateRange startP
                this.updateRange endP
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polygon s [position(q1x,q1y);endP;position(q2x,q2y)]

        /// Records bounds or emits a polyline with a triangular arrowhead.
        member this.polyLineTriangleArrow (lineStyle:Style,arrowStyle:Style,lineWidth:float,arrowSize:float) (pp:list<position>) =
            let pi = 3.14159265358979
            let startP = pp[pp.Length-2]
            let endP = pp[pp.Length-1]
            let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
            let q1x = endP.x + arrowSize*cos(t0-15.0*pi/180.0)
            let q1y = endP.y + arrowSize*sin(t0-15.0*pi/180.0)
            let q2x = endP.x + arrowSize*cos(t0+15.0*pi/180.0)
            let q2y = endP.y + arrowSize*sin(t0+15.0*pi/180.0)
            let ux,uy =
                let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
                endP.x + (startP.x-endP.x)*c,
                endP.y + (startP.y-endP.y)*c
            if writeMode then
                this.polyLine (lineStyle+Style[stroke.width lineWidth]) <| (List.map (fun i -> pp[i]) [0..pp.Length-2])@[position(ux,uy)]
            else
                for p in pp do
                    this.updateRange p
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polygon arrowStyle [position(q1x,q1y);endP;position(q2x,q2y)]

        /// Records bounds or emits a line with an arrowhead.
        member this.lineArrow (s:Style,lineWidth:float,arrowSize:float) (startP:position) (endP:position) =
            let pi = 3.14159265358979
            let t0 = atan2 (startP.y-endP.y) (startP.x-endP.x)
            let q1x = endP.x + arrowSize*cos(t0-15.0*pi/180.0)
            let q1y = endP.y + arrowSize*sin(t0-15.0*pi/180.0)
            let q2x = endP.x + arrowSize*cos(t0+15.0*pi/180.0)
            let q2y = endP.y + arrowSize*sin(t0+15.0*pi/180.0)
            let ux,uy =
                let c = lineWidth/sqrt((endP.x-startP.x)*(endP.x-startP.x)+(endP.y-startP.y)*(endP.y-startP.y))
                endP.x + (startP.x-endP.x)*c,
                endP.y + (startP.y-endP.y)*c
            if writeMode then
                this.line (s+Style[stroke.width lineWidth]) startP (position(ux,uy))
            else
                this.updateRange startP
                this.updateRange endP
                this.updateRange(position(q1x,q1y))
                this.updateRange(position(q2x,q2y))
            this.polyLine s [position(q1x,q1y);endP;position(q2x,q2y)]

    /// Adds HTML generation helpers to Aqualis contexts.
    [<AutoOpen>]
    module CompilationEnvironmentHtmlExtensions =
        type Aqualis with
            /// Gets the HTML writer bound to this context.
            member this.html = html(this)
