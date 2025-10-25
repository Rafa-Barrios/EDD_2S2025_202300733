unit userInbox;

{$MODE DELPHI}

interface
procedure ShowUserInboxWindow;
procedure ReloadInbox; // ✅ lo exportamos para que userMessage lo use

implementation

uses
  gtk2, glib2, SysUtils,
  variables, userHome, doubleLinkedList, jsonTools, userMessage;

var
  inboxWindow: PGtkWidget;
  btnClose: PGtkWidget;
  lblUnreadCount: PGtkWidget;
  treeView: PGtkWidget;
  listStore: PGtkListStore;

// ------------------------------
// Cerrar y volver al home
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(inboxWindow);
  ShowUserHomeWindow;
end;

// ------------------------------
// Doble clic en un correo
// ------------------------------
procedure OnRowActivated(tree_view: PGtkTreeView; path: PGtkTreePath;
                         column: PGtkTreeViewColumn; data: gpointer); cdecl;
var
  model: PGtkTreeModel;
  iter: TGtkTreeIter;
  estado, asunto, remitente: PChar;
  current: PMailNode;
begin
  model := gtk_tree_view_get_model(tree_view);

  if gtk_tree_model_get_iter(model, @iter, path) then
  begin
    gtk_tree_model_get(model, @iter,
      0, @estado,
      1, @asunto,
      2, @remitente,
      -1);

    // Buscar en la lista doble el mail que coincide
    current := DL_GetHead;
    while current <> nil do
    begin
      if (current^.subject = StrPas(asunto)) and
         (current^.sender = StrPas(remitente)) then
      begin
        ShowUserMessageWindow(current); // ✅ abrir mensaje
        Break;
      end;
      current := current^.Next;
    end;
  end;
end;

// ------------------------------
// Cargar correos a la tabla
// ------------------------------
procedure LoadMailsToTreeView;
var
  iter: TGtkTreeIter;
  current: PMailNode;
  unreadCount: Integer;
begin
  gtk_list_store_clear(listStore);

  unreadCount := 0;
  current := DL_GetHead;
  while current <> nil do
  begin
    gtk_list_store_append(listStore, @iter);
    gtk_list_store_set(listStore, @iter,
      0, PChar(current^.estado),
      1, PChar(current^.subject),
      2, PChar(current^.sender),
      -1);

    if current^.estado = 'NL' then
      Inc(unreadCount);

    current := current^.Next;
  end;

  gtk_label_set_text(GTK_LABEL(lblUnreadCount),
    PChar('No leídos: ' + IntToStr(unreadCount)));
end;

// ------------------------------
// Recargar la bandeja
// ------------------------------
procedure ReloadInbox;
begin
  LoadMailsToTreeView;
end;

// ------------------------------
// Mostrar la bandeja de entrada
// ------------------------------
procedure ShowUserInboxWindow;
var
  vbox, hboxTop: PGtkWidget;
  scrolledWindow: PGtkWidget;
  col: PGtkTreeViewColumn;
  cell: PGtkCellRenderer;
begin
  gtk_init(@argc, @argv);

  // Ventana principal
  inboxWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(inboxWindow), 'Bandeja de Entrada');
  gtk_container_set_border_width(GTK_CONTAINER(inboxWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(inboxWindow), 600, 400);

  // Contenedor principal
  vbox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(inboxWindow), vbox);

  // Barra superior
  hboxTop := gtk_hbox_new(False, 5);
  lblUnreadCount := gtk_label_new('No leídos: 0');
  gtk_box_pack_end(GTK_BOX(hboxTop), lblUnreadCount, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxTop, False, False, 0);

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
  col := gtk_tree_view_column_new_with_attributes('Estado', cell, 'text', 0, nil);
  gtk_tree_view_column_set_min_width(col, 80);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Asunto', cell, 'text', 1, nil);
  gtk_tree_view_column_set_min_width(col, 250);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  cell := gtk_cell_renderer_text_new;
  col := gtk_tree_view_column_new_with_attributes('Remitente', cell, 'text', 2, nil);
  gtk_tree_view_column_set_min_width(col, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), col);

  // Evento doble clic
  g_signal_connect(treeView, 'row-activated', G_CALLBACK(@OnRowActivated), nil);

  // Botón cerrar
  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnClose, False, False, 0);

  // Cargar correos desde JSON
  DL_ClearList;
  LoadInboxFromJson(json_file_mails, current_user_email);
  LoadMailsToTreeView;

  // Mostrar todo
  gtk_widget_show_all(inboxWindow);

  g_signal_connect(inboxWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.
