unit scheduled;

{$MODE DELPHI}

interface
procedure ShowScheduledWindow;

implementation

uses
  gtk2, glib2, SysUtils, Classes,
  variables, userHome, cola, jsonTools;

var
  scheduledWindow: PGtkWidget;
  btnClose, btnEnviar: PGtkWidget;
  treeView: PGtkWidget;
  listStore: PGtkListStore;

// ------------------------------
// Cerrar y volver al home
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(scheduledWindow);
  ShowUserHomeWindow;
end;

// ------------------------------
// Cargar datos de la cola en el TreeView
// ------------------------------
procedure LoadScheduledToTreeView;
var
  iter: TGtkTreeIter;
  current: PMailQueueNode;
begin
  gtk_list_store_clear(listStore); // limpiar tabla

  current := Queue_Peek;
  while current <> nil do
  begin
    gtk_list_store_append(listStore, @iter);
    gtk_list_store_set(listStore, @iter,
      0, PChar(current^.sender),
      1, PChar(current^.subject),
      2, PChar(current^.dateScheduled),
      -1);

    current := current^.Next;
  end;
end;

// ------------------------------
// Enviar correo programado
// ------------------------------
procedure OnEnviarClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  nextMail: PMailQueueNode;
  newId: Integer;
  fecha: String;
begin
  if Queue_IsEmpty then
  begin
    Writeln('No hay correos para enviar.');
    Exit;
  end;

  nextMail := Queue_Peek;

  // Solo enviar si el correo pertenece al usuario actual
  if not SameText(nextMail^.sender, current_user_email) then
  begin
    Writeln('El siguiente correo no pertenece al usuario actual.');
    Exit;
  end;

  // Generar un ID único entre 1–25 en mails.json
  newId := GetAvailableId('/home/rafa/Documents/EDD_2S2025_202300733/Fase2/test/mails.json', 25);

  // Fecha actual
  fecha := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  // Guardar en mails.json (ruta corregida)
  if AddMailToJson('/home/rafa/Documents/EDD_2S2025_202300733/Fase2/test/mails.json',
                   nextMail^.sender, nextMail^.recipient,
                   nextMail^.subject, nextMail^.message,
                   newId) then
  begin
    // Eliminar correo de scheduled.json
    RemoveScheduledMailFromJson('/home/rafa/Documents/EDD_2S2025_202300733/Fase2/test/scheduled.json', nextMail^.id);

    // Eliminar de la cola en memoria
    Queue_Dequeue;

    Writeln('Correo enviado correctamente al destinatario.');
  end
  else
    Writeln('Error al enviar el correo.');

  // Refrescar TreeView con la cola actualizada
  LoadScheduledToTreeView;
end;

// ------------------------------
// Mostrar ventana de correos programados
// ------------------------------
procedure ShowScheduledWindow;
var
  vbox: PGtkWidget;
  scrolledWindow: PGtkWidget;
  col: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
begin
  gtk_init(@argc, @argv);

  // 🔹 Cargar la cola desde scheduled.json
  LoadScheduledFromJson('/home/rafa/Documents/EDD_2S2025_202300733/Fase2/test/scheduled.json', current_user_email);

  // Ventana principal
  scheduledWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(scheduledWindow), 'Correos Programados');
  gtk_container_set_border_width(GTK_CONTAINER(scheduledWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(scheduledWindow), 600, 400);

  // Contenedor principal
  vbox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(scheduledWindow), vbox);

  // Botón Enviar
  btnEnviar := gtk_button_new_with_label('Enviar');
  g_signal_connect(btnEnviar, 'clicked', G_CALLBACK(@OnEnviarClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnEnviar, False, False, 0);

  // Tabla de correos
  scrolledWindow := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolledWindow, True, True, 0);

  treeView := gtk_tree_view_new;
  gtk_container_add(GTK_CONTAINER(scrolledWindow), treeView);

  listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeView), GTK_TREE_MODEL(listStore));

  // Columnas
  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Remitente', cell, 'text', 0, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Asunto', cell, 'text', 1, nil);
  gtk_tree_view_column_set_min_width(col, 250);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Fecha Programada', cell, 'text', 2, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // 🔹 Cargar datos desde la cola al TreeView
  LoadScheduledToTreeView;

  // Botón cerrar (abajo)
  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnClose, False, False, 0);

  // Mostrar todo
  gtk_widget_show_all(scheduledWindow);

  g_signal_connect(scheduledWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.
