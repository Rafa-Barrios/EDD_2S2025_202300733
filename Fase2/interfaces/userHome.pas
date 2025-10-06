unit userHome;

interface
    procedure ShowUserHomeWindow;

implementation

uses
    gtk2, glib2, SysUtils, login,
    userMailSend,
    userMailProgram,
    userNewContact,
    userViewContact,
    userUpdate,
    userInbox,
    userPapelera,
    scheduled,
    userDeleteContact,
    userFavorites,
    userDraft,
    userCommunitySend,
    avltree,
    btree,
    variables, filesTools, jsonTools, interfaceTools,
    doubleLinkedList;

var
    userWindow: PGtkWidget;
    lblWelcome: PGtkWidget;
    btnInbox, btnSendMail, btnTrash, btnScheduleMail,
    btnScheduled, btnAddContact, btnDeleteContact, btnContacts,
    btnUpdateProfile, btnReports, btnFavorites, btnDrafts, btnCommunityMsgs,
    btnLogout: PGtkWidget;

procedure OnInboxClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserInboxWindow;
end;

procedure OnSendMailClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserMailSendWindow;
end;

procedure OnTrashClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserPapeleraWindow;
end;

procedure OnScheduleMailClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserMailProgramWindow;
end;

procedure OnScheduledClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowScheduledWindow;
end;

procedure OnAddContactClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserNewContactWindow;
end;

procedure OnDeleteContactClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserDeleteContactWindow;
end;

procedure OnContactsClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserViewContactWindow;
end;

procedure OnUpdateProfileClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserUpdateWindow;
end;

procedure OnFavoritesClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserFavoritesWindow;
end;

procedure OnDraftClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserDraftsWindow;
end;

procedure OnComClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(userWindow);
    ShowUserCommunitySendWindow;
end;

// 🔹 NUEVO: Generar reportes (incluye borradores ordenados por ID)
procedure OnReportsClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    nameFolder: AnsiString;
    okAVL: Boolean;
    okBTree: Boolean;
begin
    nameFolder := current_user_username + '-Reports';
    // tu reporte existente (usuarios/comunidades)
    filesTools.GenerateReports('mails', nameFolder, DL_GenerateDot);

    // Generar AVL a partir de drafts (avltree.pas)
    okAVL := GenerateAVLReportFromDrafts(nameFolder);

    // 🔹 Generar BTree a partir de favoritos
    okBTree := GenerateBTreeReportFromFavorites(nameFolder);

    // Mensaje al usuario
    if okAVL and okBTree then
      ShowSuccessMessage(nil, 'Reportes', 'Gráficos AVL (drafts) y BTree (favoritos) generados correctamente en ' + nameFolder)
    else if okAVL then
      ShowErrorMessage(nil, 'Reportes', 'Solo se generó el gráfico AVL (drafts). Revisa BTree.')
    else if okBTree then
      ShowErrorMessage(nil, 'Reportes', 'Solo se generó el gráfico BTree (favoritos). Revisa AVL.')
    else
      ShowErrorMessage(nil, 'Reportes', 'No se pudieron generar los gráficos. Verifica Graphviz.');
end;

procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    DL_ClearList;
    gtk_widget_destroy(userWindow);
    ShowLoginWindow;
end;

procedure ShowUserHomeWindow;
var
    grid: PGtkWidget;
    welcomeText: AnsiString; 
begin
    gtk_init(@argc, @argv);

    userWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(userWindow), 'Inicio Usuario');
    gtk_container_set_border_width(GTK_CONTAINER(userWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(userWindow), 450, 800);

    grid := gtk_table_new(14, 1, False);
    gtk_container_add(GTK_CONTAINER(userWindow), grid);

    welcomeText := 'Bienvenido: ' + current_user_username;
    lblWelcome := gtk_label_new(PChar(welcomeText));
    gtk_table_attach_defaults(GTK_TABLE(grid), lblWelcome, 0, 1, 0, 1);

    // Botones
    btnInbox := gtk_button_new_with_label('Bandeja de entrada');
    btnSendMail := gtk_button_new_with_label('Enviar correo');
    btnTrash := gtk_button_new_with_label('Papelera');
    btnScheduleMail := gtk_button_new_with_label('Programar correos');
    btnScheduled := gtk_button_new_with_label('Correos programados');
    btnAddContact := gtk_button_new_with_label('Agregar contactos');
    btnDeleteContact := gtk_button_new_with_label('Eliminar Contacto');
    btnContacts := gtk_button_new_with_label('Contactos');
    btnUpdateProfile := gtk_button_new_with_label('Actualizar perfil');
    btnReports := gtk_button_new_with_label('Generar reportes');
    btnFavorites := gtk_button_new_with_label('Favoritos');
    btnDrafts := gtk_button_new_with_label('Borradores');
    btnCommunityMsgs := gtk_button_new_with_label('Mensajes a Comunidad');
    btnLogout := gtk_button_new_with_label('Cerrar sesión');

    // Conexión de eventos
    g_signal_connect(btnInbox, 'clicked', G_CALLBACK(@OnInboxClick), nil);
    g_signal_connect(btnSendMail, 'clicked', G_CALLBACK(@OnSendMailClick), nil);
    g_signal_connect(btnTrash, 'clicked', G_CALLBACK(@OnTrashClick), nil);
    g_signal_connect(btnScheduleMail, 'clicked', G_CALLBACK(@OnScheduleMailClick), nil);
    g_signal_connect(btnScheduled, 'clicked', G_CALLBACK(@OnScheduledClick), nil);
    g_signal_connect(btnAddContact, 'clicked', G_CALLBACK(@OnAddContactClick), nil);
    g_signal_connect(btnDeleteContact, 'clicked', G_CALLBACK(@OnDeleteContactClick), nil);
    g_signal_connect(btnContacts, 'clicked', G_CALLBACK(@OnContactsClick), nil);
    g_signal_connect(btnUpdateProfile, 'clicked', G_CALLBACK(@OnUpdateProfileClick), nil);
    g_signal_connect(btnReports, 'clicked', G_CALLBACK(@OnReportsClick), nil);
    g_signal_connect(btnFavorites, 'clicked', G_CALLBACK(@OnFavoritesClick), nil);
    g_signal_connect(btnDrafts, 'clicked', G_CALLBACK(@OnDraftClick), nil);
    g_signal_connect(btnCommunityMsgs, 'clicked', G_CALLBACK(@OnComClick), nil);
    g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

    // Ubicación en tabla
    gtk_table_attach_defaults(GTK_TABLE(grid), btnInbox,         0, 1, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnSendMail,      0, 1, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnTrash,         0, 1, 3, 4);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnScheduleMail,  0, 1, 4, 5);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnScheduled,     0, 1, 5, 6);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnAddContact,    0, 1, 6, 7);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnDeleteContact, 0, 1, 7, 8);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnContacts,      0, 1, 8, 9);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnUpdateProfile, 0, 1, 9, 10);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnReports,       0, 1, 10, 11);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnFavorites,     0, 1, 11, 12);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnDrafts,        0, 1, 12, 13);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCommunityMsgs, 0, 1, 13, 14);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnLogout,        0, 1, 14, 15);

    gtk_widget_show_all(userWindow);
    g_signal_connect(userWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
    gtk_main;
end;

end.
