namespace StudentContact
open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.Forms

[<JavaScript>]
module ContactManager =

    type Contact = {
        firstName: string
        lastName: string
        email: string
        phone: string
        address: string
    }

    let ContactForm (init: Contact) =
        Form.Return (fun first last email phone address ->
            { firstName = first
              lastName = last
              email = email
              phone = phone
              address = address
            })
        <*> (Form.Yield init.firstName
            |> Validation.IsNotEmpty "Please enter the first name.")
        <*> (Form.Yield init.lastName
            |> Validation.IsNotEmpty "Please enter the last name.")
        <*> (Form.Yield init.email
            |> Validation.IsMatch "^\w+([-+.']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*$" "Invalid email address.")
        <*> (Form.Yield init.phone
            |> Validation.IsMatch "^[0-9]+$" "Invalid phone number format.")
        <*> Form.Yield init.address
        |> Form.WithSubmit

    let ShowErrorsFor v =
        v
        |> View.Map (function
            | Success _ -> Doc.Empty
            | Failure errors ->
                Doc.Concat [
                    for error in errors do
                        yield b [attr.style "color:red"] [text error.Text]
                ]
        )
        |> Doc.EmbedView

    let contactList = Var.Create([]: Contact list)
    let editIndex = Var.Create(None: int option)
    let modalVisible = Var.Create(false)

    let showModal () = modalVisible.Value <- true

    let hideModal () = modalVisible.Value <- false

    let RenderContact (firstName: Var<string>)
                      (lastName: Var<string>)
                      (email: Var<string>)
                      (phone: Var<string>)
                      (address: Var<string>)                      
                      (submit: Submitter<Result<_>>) =
        section [attr.``class`` "section"] [
            h2 [attr.``class`` "subtitle"] [text "Student Contact"]
            p [attr.``class`` "subhead"] [text "A simple Website that stores, Updates and Deletes Student Contact Information"]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "First name: "
                    Doc.InputType.Text [attr.``class`` "input"] firstName
                ]
                ShowErrorsFor (submit.View.Through firstName)
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "Last name: "
                    Doc.InputType.Text [attr.``class`` "input"] lastName
                ]
                ShowErrorsFor (submit.View.Through lastName)
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "Email: "
                    Doc.InputType.Email [attr.``class`` "input"] email
                ]
                ShowErrorsFor (submit.View.Through email)
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "Phone: "
                    Doc.InputType.Text [attr.``class`` "input"] phone
                ]
                ShowErrorsFor (submit.View.Through phone)
            ]
            div [attr.``class`` "field"] [
                label [attr.``class`` "label"] [
                    text "Address: "
                    Doc.InputType.Text [attr.``class`` "input"] address
                ]
                ShowErrorsFor (submit.View.Through address)
            ]
            div [] [
                Doc.Button "Submit" [attr.``class`` "button"] submit.Trigger
            ]
            div [] [
                Doc.Button "View Contacts" [attr.``class`` "button"] (fun () -> showModal())
            ]
            Doc.EmbedView (
                modalVisible.View |> View.Map (fun visible ->
                    if visible then
                        div [attr.``class`` "modal is-active"] [
                            div [attr.``class`` "modal-background"] [Doc.Button "" [] (fun () -> hideModal())]
                            div [attr.``class`` "modal-content"] [
                                div [attr.``class`` "box"] [
                                    h2 [attr.``class`` "subtitle"] [text "Contact List"]
                                    contactList.View.Doc(function
                                        | [] -> p [] [text "No Contacts available"]
                                        | contacts ->
                                            ul [attr.``class`` "listing"] (contacts |> List.mapi (fun index contact ->
                                                li [] [
                                                    text (sprintf "* - %s %s (%s, %s, %s)" contact.firstName contact.lastName contact.email contact.phone contact.address)
                                                    Doc.Button "Edit" [attr.``class`` "button is-small"] (fun () ->
                                                        editIndex.Value <- Some index
                                                        firstName.Value <- contact.firstName
                                                        lastName.Value <- contact.lastName
                                                        email.Value <- contact.email
                                                        phone.Value <- contact.phone
                                                        address.Value <- contact.address
                                                    )
                                                    Doc.Button "Delete" [attr.``class`` "button is-small"] (fun () ->
                                                        contactList.Value <- List.mapi (fun i c -> if i <> index then Some c else None) contactList.Value |> List.choose id
                                                    )
                                                ]
                                            ))
                                    )
                                ]
                            ]
                            div [attr.``class`` "modal-close is-large"] [Doc.Button "" [] (fun () -> hideModal())]
                        ]
                    else Doc.Empty
                )
            )
        ]

    [<SPAEntryPoint>]
    let Main() =
        let mutable formInit = {
            firstName = ""
            lastName = ""
            email = ""
            phone = ""
            address = ""
        }

        let form = ContactForm formInit
        form
        |> Form.Run (fun p ->
            match editIndex.Value with
            | Some index ->
                contactList.Value <- List.mapi (fun i c -> if i = index then p else c) contactList.Value
                editIndex.Value <- None
            | None ->
                contactList.Value <- p :: contactList.Value
            let message = "Your Contact: " + p.firstName + " " + p.lastName + "!"
            JS.Alert message
            formInit <- { firstName = ""; lastName = ""; email = ""; phone = ""; address = "" }
        )
        |> Form.Render RenderContact
        |> Doc.RunById "main"