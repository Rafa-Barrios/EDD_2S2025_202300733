unit userMailSend;

interface
    procedure ShowUserMailSendWindow;

implementation

{$MODE DELPHI}

uses
    gtk2, glib2,
    SysUtils,
    userHome,
    variables, interfaceTools,
    jsonTools, doubleLinkedList;

var
    mailSendWindow: PGtkWidget;
    lblRecipient, lblSubject, lblMessage: PGtkWidget;
    entryRecipient, entrySubject, entryMessage: PGtkWidget;
    btnSend, btnCancel, btnDraft: PGtkWidget;

    usedIDs: array of Integer;

// ---------------------------
// Generar un ID único entre 1 y 25
// ---------------------------
function GenerateUniqueID: Integer;
var
    candidate, i: Integer;
    found: Boolean;
begin
    Randomize;
    repeat
        candidate := Random(25) + 1;
        found := False;
        for i := 0 to High(usedIDs) do
            if usedIDs[i] = candidate then
            begin
                found := True;
                Break;
            end;
    until not found;

    SetLength(usedIDs, Length(usedIDs) + 1);
    usedIDs[High(usedIDs)] := candidate;

    GenerateUniqueID := candidate;
end;

// ---------------------------
// Evento Enviar correo
// ---------------------------
procedure OnSendClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    recipientEmail, subject, message: string;
    buffer: PGtkTextBuffer;
    iterStart, iterEnd: TGtkTextIter;
    mailID: Integer;
begin
    recipientEmail := gtk_entry_get_text(GTK_ENTRY(entryRecipient));
    subject := gtk_entry_get_text(GTK_ENTRY(entrySubject));

    buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(entryMessage));
    gtk_text_buffer_get_start_iter(buffer, @iterStart);
    gtk_text_buffer_get_end_iter(buffer, @iterEnd);
    message := gtk_text_buffer_get_text(buffer, @iterStart, @iterEnd, True);

    if (recipientEmail = '') or (subject = '') or (message = '') then
    begin
        ShowErrorMessage(mailSendWindow, 'Error', 'Por favor complete todos los campos.');
        Exit;
    end;

    mailID := GenerateUniqueID;

    if AddMailToJson(json_file_mails, current_user_email, recipientEmail, subject, message, mailID) then
    begin
        DL_InsertToList(IntToStr(mailID), current_user_email, subject, message, DateTimeToStr(Now), 'EN');
        ShowSuccessMessage(mailSendWindow, 'Éxito', 'Correo enviado correctamente.');
        gtk_widget_destroy(mailSendWindow);
        ShowUserHomeWindow;
    end
    else
        ShowErrorMessage(mailSendWindow, 'Error', 'Error al enviar el correo.');
end;

// ---------------------------
// Evento Guardar en Borrador
// ---------------------------
procedure OnDraftClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    recipientEmail, subject, message: string;
    buffer: PGtkTextBuffer;
    iterStart, iterEnd: TGtkTextIter;
    mailID: Integer;
begin
    recipientEmail := gtk_entry_get_text(GTK_ENTRY(entryRecipient));
    subject := gtk_entry_get_text(GTK_ENTRY(entrySubject));

    buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(entryMessage));
    gtk_text_buffer_get_start_iter(buffer, @iterStart);
    gtk_text_buffer_get_end_iter(buffer, @iterEnd);
    message := gtk_text_buffer_get_text(buffer, @iterStart, @iterEnd, True);

    if (recipientEmail = '') or (subject = '') or (message = '') then
    begin
        ShowErrorMessage(mailSendWindow, 'Error', 'Por favor complete todos los campos.');
        Exit;
    end;

    mailID := GenerateUniqueID;

    // Guardar en JSON de borradores
    if AddMailToJson(json_file_drafts, current_user_email, recipientEmail, subject, message, mailID) then
    begin
        // Insertar en lista doble con estado = 'DR' (Draft)
        DL_InsertToList(IntToStr(mailID), current_user_email, subject, message, DateTimeToStr(Now), 'DR');
        ShowSuccessMessage(mailSendWindow, 'Borrador', 'Correo guardado como borrador.');
        gtk_widget_destroy(mailSendWindow);
        ShowUserHomeWindow;
    end
    else
        ShowErrorMessage(mailSendWindow, 'Error', 'No se pudo guardar el borrador.');
end;

// ---------------------------
// Evento Cancelar
// ---------------------------
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(mailSendWindow);
    ShowUserHomeWindow;
end;

// ---------------------------
// Mostrar ventana
// ---------------------------
procedure ShowUserMailSendWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    mailSendWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(mailSendWindow), 'Enviar Correo');
    gtk_container_set_border_width(GTK_CONTAINER(mailSendWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(mailSendWindow), 450, 400);

    grid := gtk_table_new(5, 2, False);
    gtk_container_add(GTK_CONTAINER(mailSendWindow), grid);

    lblRecipient := gtk_label_new('Destinatario:');
    entryRecipient := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblRecipient, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryRecipient, 1, 2, 0, 1);

    lblSubject := gtk_label_new('Asunto:');
    entrySubject := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblSubject, 0, 1, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), entrySubject, 1, 2, 1, 2);

    lblMessage := gtk_label_new('Mensaje:');
    entryMessage := gtk_text_view_new;
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(entryMessage), GTK_WRAP_WORD);
    gtk_widget_set_size_request(entryMessage, 300, 150);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblMessage, 0, 1, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryMessage, 1, 2, 2, 3);

    // Botones
    btnCancel := gtk_button_new_with_label('Cancelar');
    btnDraft := gtk_button_new_with_label('Borrador');
    btnSend := gtk_button_new_with_label('Enviar');

    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
    g_signal_connect(btnDraft, 'clicked', G_CALLBACK(@OnDraftClick), nil);
    g_signal_connect(btnSend, 'clicked', G_CALLBACK(@OnSendClick), nil);

    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 3, 4);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnDraft, 0, 1, 4, 5);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnSend, 1, 2, 3, 5);

    gtk_widget_show_all(mailSendWindow);
    g_signal_connect(mailSendWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
    gtk_main;
end;

end.
