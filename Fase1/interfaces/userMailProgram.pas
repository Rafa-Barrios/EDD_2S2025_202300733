unit userMailProgram;

interface
    procedure ShowUserMailProgramWindow;

implementation

uses
    gtk2, glib2,
    SysUtils, DateUtils,
    userHome,
    variables, interfaceTools,
    jsonTools, cola;  // usamos la cola y JSON

var
    mailProgramWindow: PGtkWidget;
    lblRecipient, lblSubject, lblMessage, lblDate: PGtkWidget;
    entryRecipient, entrySubject, entryDate: PGtkWidget;
    entryMessage: PGtkWidget;
    btnProgram, btnCancel: PGtkWidget;

// ---------------------------
// Evento Programar correo
// ---------------------------
procedure OnProgramClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    recipientEmail, subject, message, dateStr, id: string;
    buffer: PGtkTextBuffer;
    iterStart, iterEnd: TGtkTextIter;
    saveOk: Boolean;
begin
    // Leer campos de entrada
    recipientEmail := gtk_entry_get_text(GTK_ENTRY(entryRecipient));
    subject := gtk_entry_get_text(GTK_ENTRY(entrySubject));
    dateStr := gtk_entry_get_text(GTK_ENTRY(entryDate));

    buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(entryMessage));
    gtk_text_buffer_get_start_iter(buffer, @iterStart);
    gtk_text_buffer_get_end_iter(buffer, @iterEnd);
    message := gtk_text_buffer_get_text(buffer, @iterStart, @iterEnd, True);

    // Validar campos
    if (recipientEmail = '') or (subject = '') or (message = '') or (dateStr = '') then
    begin
        ShowErrorMessage(mailProgramWindow, 'Error', 'Por favor complete todos los campos.');
        Exit;
    end;

    // Generar ID único usando timestamp Unix
    id := IntToStr(DateTimeToUnix(Now));

    // Encolar el mensaje programado
    // Parámetros: id, emisor, receptor, asunto, mensaje, fecha
    Queue_Enqueue(id, current_user_email, recipientEmail, subject, message, dateStr);

    // Guardar cola en JSON
    saveOk := SaveScheduledToJson(json_file_scheduled, current_user_email);

    if not saveOk then
    begin
        ShowErrorMessage(mailProgramWindow, 'Error', 'Error al guardar el correo programado.');
        Exit;
    end;

    ShowSuccessMessage(mailProgramWindow, 'Éxito', 'Correo programado correctamente.');

    // Cerrar ventana y volver a Home
    gtk_widget_destroy(mailProgramWindow);
    ShowUserHomeWindow;
end;

// ---------------------------
// Evento Cancelar -> vuelve a la ventana principal
// ---------------------------
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(mailProgramWindow);
    ShowUserHomeWindow;
end;

// ---------------------------
// Mostrar ventana de programación de correo
// ---------------------------
procedure ShowUserMailProgramWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    // Crear ventana principal
    mailProgramWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(mailProgramWindow), 'Programar Correo');
    gtk_container_set_border_width(GTK_CONTAINER(mailProgramWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(mailProgramWindow), 450, 450);

    // Crear tabla para organizar etiquetas y entradas (5 filas x 2 columnas)
    grid := gtk_table_new(5, 2, False);
    gtk_container_add(GTK_CONTAINER(mailProgramWindow), grid);

    // Etiqueta y entrada para Destinatario
    lblRecipient := gtk_label_new('Destinatario:');
    entryRecipient := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblRecipient, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryRecipient, 1, 2, 0, 1);

    // Etiqueta y entrada para Asunto
    lblSubject := gtk_label_new('Asunto:');
    entrySubject := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblSubject, 0, 1, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), entrySubject, 1, 2, 1, 2);

    // Etiqueta y campo para Mensaje (TextView grande)
    lblMessage := gtk_label_new('Mensaje:');
    entryMessage := gtk_text_view_new;
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(entryMessage), GTK_WRAP_WORD);
    gtk_widget_set_size_request(entryMessage, 300, 150);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblMessage, 0, 1, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryMessage, 1, 2, 2, 3);

    // Etiqueta y entrada para Fecha
    lblDate := gtk_label_new('Fecha programada:');
    entryDate := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblDate, 0, 1, 3, 4);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryDate, 1, 2, 3, 4);

    // Botones Programar y Cancelar en la última fila
    btnProgram := gtk_button_new_with_label('Programar');
    btnCancel := gtk_button_new_with_label('Cancelar');
    g_signal_connect(btnProgram, 'clicked', G_CALLBACK(@OnProgramClick), nil);
    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 4, 5);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnProgram, 1, 2, 4, 5);

    // Mostrar todos los widgets
    gtk_widget_show_all(mailProgramWindow);

    // Evento para cerrar ventana
    g_signal_connect(mailProgramWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    gtk_main;
end;

end.
