unit uDiagramaManager;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleotério
Data: 17/10/2013
}

interface

uses
  LCLIntf, LCLType, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, DB, BufDataset, SysUtils, fgl,
  uAppFile, uERNotationsCore, uFrameConsultaDados, uVariaveisGlobais;

type

  { TDiagramaManager }

  TDiagramaManager = class
  private
    FOpenedFile: string;
    FListEntityContainerCarregados: TStringList;
    FListMenusDeDiagramasAbertos: specialize TFPGMapObject<string, TMenuItem>;
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    FParentEntityContainer: TWinControl;
    FEntityContainerCorrente: TEntityContainer;
    FOnMudancaEstadoModelo: TNotifyEvent;
    FMenuItemParaDiagramasAbertos: TMenuItem;
    FAmostraDadosCorrente: TFrameConsultaDados;
    FContainerAnteior: TObject;
    // client data set com os nomes dos diagramas
    FCdsDiagramas: TBufDataSet;

    procedure WriteFile;
    procedure RenderizarDiagrama(Id: string; entityContainer: TEntityContainer);
    procedure FreeAllEntityContainer;

    procedure ClickMenuItemDiagAberto(Sender: TObject);
    procedure OpenGenericContainer(Id: string; container: TObject);
    // acesso a propriedades
    function GetTemModeloParaSalvar: boolean;
    procedure CreateMenuRapido(diagramaId, captionMenuItem: string);
    function TestDBCnn: boolean;
    procedure PrepareOpenContainer;
  public
    constructor Create(ParentEntityContainer: TWinControl);
    destructor Destroy; override;
    procedure NovoModelo;
    procedure FecharModelo;
    procedure OpenModelo(DBDataFileName: string);
    function SaveModelo: boolean;
    procedure OpenEntityContainer(Id: string);
    procedure OpenAmostraContainer(OwnerTabela: string);
    procedure RemoverDiagrama(Id: string);
    procedure RenomearDiagrama(Id: string);
    procedure NovoDiagrama;
    procedure RemoveContainerDaListaCarregados(Id: string);
  published
    property CdsDiagramas: TBufDataSet read FCdsDiagramas write FCdsDiagramas;
    property EntityContainerCorrente: TEntityContainer read FEntityContainerCorrente;
    property AmostraDadosCorrente: TFrameConsultaDados read FAmostraDadosCorrente;
    property OnMudancaEstadoModelo: TNotifyEvent
      read FOnMudancaEstadoModelo write FOnMudancaEstadoModelo;
    property TemModeloParaSalvar: boolean read GetTemModeloParaSalvar;
    property NomeModeloAberto: string read FOpenedFile;
    property MenuItemParaDiagramasAbertos: TMenuItem read FMenuItemParaDiagramasAbertos write FMenuItemParaDiagramasAbertos;
  end;

implementation

uses uPrincipal, uConnection;


  { TDiagramaManager }

procedure TDiagramaManager.FreeAllEntityContainer;
var
  i: integer;
begin
  // remove todos os entity containers da memória
  for i := FListEntityContainerCarregados.Count - 1 downto 0 do
    TObject(FListEntityContainerCarregados.Objects[i]).Free;
  FListEntityContainerCarregados.Clear;

  // limpa o client dataset axiliar
  FCdsDiagramas.First;
  while not FCdsDiagramas.EOF do
    FCdsDiagramas.Delete;

  // remove o entity container corrente
  FEntityContainerCorrente := nil;

  // remove todos os menus de diagramas abertos
  for i := FListMenusDeDiagramasAbertos.Count - 1 downto 0 do
  begin
    if Assigned(FMenuItemParaDiagramasAbertos) then
      FMenuItemParaDiagramasAbertos.Remove(FListMenusDeDiagramasAbertos.Data[i]);

    FListMenusDeDiagramasAbertos.Data[i].Free;
    FListMenusDeDiagramasAbertos.Delete(i);
  end;
end;

function TDiagramaManager.GetTemModeloParaSalvar: boolean;
begin
  Result := Assigned(AppFile);
end;

procedure TDiagramaManager.ClickMenuItemDiagAberto(Sender: TObject);
var
  indexOfMenu, indexOfContainer: integer;
  id: string;
  container: TObject;
begin

  indexOfMenu := FListMenusDeDiagramasAbertos.IndexOfData(Sender as TMenuItem);
  if indexOfMenu > -1 then
  begin
    id := FListMenusDeDiagramasAbertos.Keys[indexOfMenu];
    indexOfContainer := FListEntityContainerCarregados.IndexOf(id);
    container := FListEntityContainerCarregados.Objects[indexOfContainer];

    OpenGenericContainer(id, container);
  end;
end;

constructor TDiagramaManager.Create(ParentEntityContainer: TWinControl);
begin
  inherited Create;
  FParentEntityContainer := ParentEntityContainer;
  FOpenDialog := TOpenDialog.Create(nil);
  FOpenDialog.Filter := 'Arquivos do DB-Data (*.dbdata)|*.DBDATA';

  FSaveDialog := TSaveDialog.Create(nil);
  FSaveDialog.Filter := 'Arquivos do DB-Data (*.dbdata)|*.DBDATA';

  FCdsDiagramas := TBufDataSet.Create(nil);
  FCdsDiagramas.FieldDefs.Add('titulo', ftString, 300, False);
  FCdsDiagramas.FieldDefs.Add('id', ftString, 64, False);
  FCdsDiagramas.IndexDefs.Add('idxTitulo', 'titulo', []);
  // status: N -> Novo, A -> Alterado, E -> Excluído, 0 - Carregado do XML
  FCdsDiagramas.FieldDefs.Add('status', ftString, 1, False);
  FCdsDiagramas.CreateDataSet;
  FCdsDiagramas.IndexFieldNames := 'titulo';

  FListEntityContainerCarregados := TStringList.Create;
  FListMenusDeDiagramasAbertos :=
    specialize TFPGMapObject<string, TMenuItem>.Create(False);
end;

destructor TDiagramaManager.Destroy;
begin
  FOpenDialog.Free;
  FSaveDialog.Free;
  FreeAllEntityContainer;
  FCdsDiagramas.Free;
  FListEntityContainerCarregados.Free;
  FListMenusDeDiagramasAbertos.Free;
  inherited;
end;

procedure TDiagramaManager.FecharModelo;
begin
  FOpenedFile := '';
  FreeAndNil(AppFile);
  FreeAllEntityContainer;

  if Assigned(FOnMudancaEstadoModelo) then
    FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.NovoDiagrama;
var
  titulo: string;

  Uid: TGuid;
  Result: HResult;
begin
  titulo := InputBox('Novo Diagrama', 'Digite o nome do Diagrama', '');
  if Trim(titulo) <> '' then
  begin
    Result := CreateGuid(Uid);
    if Result <> S_OK then
      raise Exception.Create('Erro ao gerar guid.');

    FCdsDiagramas.Append;
    FCdsDiagramas.FieldByName('id').AsString := GuidToString(Uid);
    FCdsDiagramas.FieldByName('titulo').AsString := titulo;
    FCdsDiagramas.FieldByName('status').AsString := 'N'; // novo
    FCdsDiagramas.Post;

    OpenEntityContainer(FCdsDiagramas.FieldByName('id').AsString);

    if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
  end
  else
    Application.MessageBox('Nome do diagrama não informado!', 'ATENÇÃO',
      MB_OK + MB_ICONSTOP);

end;

procedure TDiagramaManager.NovoModelo;
begin
  AppFile := TAppFile.Create;

  if Assigned(FOnMudancaEstadoModelo) then
    FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.OpenModelo(DBDataFileName: string);
var
  i: integer;
  diagrama: TDiagrama;
begin
  // se o nome do arquivo não vier como parâmetro, então carregar pela janela
  if DBDataFileName = '' then
  begin
    if FOpenDialog.Execute then
      FOpenedFile := FOpenDialog.FileName;
  end
  else
    FOpenedFile := DBDataFileName;

  if FOpenedFile <> '' then
  begin
    AppFile := TAppFile.Create;
    AppFile.OpenFile(FOpenedFile);

    for i := 0 to AppFile.Diagramas.Count - 1 do
    begin
      diagrama := AppFile.Diagramas[i];
      FCdsDiagramas.Append;
      FCdsDiagramas.FieldByName('id').AsString := diagrama.Id;
      FCdsDiagramas.FieldByName('titulo').AsString := diagrama.Titulo;
      FCdsDiagramas.FieldByName('status').AsString := '0';
      FCdsDiagramas.Post;
    end;
    FCdsDiagramas.First;

    if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
  end;

end;

procedure TDiagramaManager.PrepareOpenContainer;
begin
  // tira da tela o diagrama
  if FEntityContainerCorrente <> nil then
  begin
    FEntityContainerCorrente.EntityArea.Visible := False;
    FContainerAnteior := FEntityContainerCorrente;
    FEntityContainerCorrente := nil;
  end;

  // tira da tela o frame de amostra de dados
  if FAmostraDadosCorrente <> nil then
  begin
    FAmostraDadosCorrente.Visible := False;
    FContainerAnteior := FEntityContainerCorrente;
    FAmostraDadosCorrente := nil;
  end;

end;

procedure TDiagramaManager.OpenAmostraContainer(OwnerTabela: string);
var
  frameAmostra: TFrameConsultaDados;
  index: integer;
begin
  PrepareOpenContainer;

  // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
  index := FListEntityContainerCarregados.IndexOf(OwnerTabela);

  if index > -1 then
  begin
    TFrameConsultaDados(FListEntityContainerCarregados.Objects[index]).Visible := True;
    FAmostraDadosCorrente := TFrameConsultaDados(
      FListEntityContainerCarregados.Objects[index]);
  end
  else
  begin
    frameAmostra := TFrameConsultaDados.Create(FParentEntityContainer);
    frameAmostra.Parent := FParentEntityContainer;
    frameAmostra.Name := ''; // da um nome qualquer;
    FAmostraDadosCorrente := frameAmostra;
    FListEntityContainerCarregados.AddObject(OwnerTabela, TObject(frameAmostra));
    // Cria o menu para diagramas já carregados
    CreateMenuRapido(OwnerTabela, 'Query de ' + OwnerTabela);
    frameAmostra.ObterAmostra(OwnerTabela);
  end;

  if Assigned(FOnMudancaEstadoModelo) then
    FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.CreateMenuRapido(diagramaId, captionMenuItem: string);
var
  menu: TMenuItem;
begin
  if Assigned(FMenuItemParaDiagramasAbertos) then
  begin
    menu := TMenuItem.Create(FMenuItemParaDiagramasAbertos);
    menu.Caption := captionMenuItem;
    menu.OnClick := @ClickMenuItemDiagAberto;
    FListMenusDeDiagramasAbertos.Add(diagramaId, menu);
    FMenuItemParaDiagramasAbertos.Add(menu);
  end;
end;

function TDiagramaManager.TestDBCnn: boolean;
begin
  try
    TConexao.FecharConexao;
    TConexao.GetConexao;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TDiagramaManager.OpenEntityContainer(Id: string);
var
  container: TEntityContainer;
  index: integer;
begin
  if TestDBCnn then
  begin
    PrepareOpenContainer;

    // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
    index := FListEntityContainerCarregados.IndexOf(Id);

    if index > -1 then
    begin
      TEntityContainer(FListEntityContainerCarregados.Objects[index]).EntityArea.Visible :=
        True;
      FEntityContainerCorrente :=
        TEntityContainer(FListEntityContainerCarregados.Objects[index]);
    end
    else
      // se ele ainda não foi aberto, entao abre
    begin
      // testa se a conexão está funcionando
      if FCdsDiagramas.Locate('id', Id, []) then
      begin
        container := TEntityContainer.Create(FParentEntityContainer);
        container.DiagramaId := FCdsDiagramas.FieldByName('id').AsString;
        container.Titulo := FCdsDiagramas.FieldByName('titulo').AsString;
        FListEntityContainerCarregados.AddObject(Id, TObject(container));
        FEntityContainerCorrente := container;
        // Cria o menu para diagramas já carregados
        CreateMenuRapido(id, container.Titulo);
        // só abre os diagramas já carregados do disco (0) e muda o status para alterado (A)
        // pois todos os demais entrarão na lógica do blodo dos diagramas já em memória
        if FCdsDiagramas.FieldByName('status').AsString = '0' then
        begin
          RenderizarDiagrama(Id, container);
          FCdsDiagramas.Edit;
          FCdsDiagramas.FieldByName('status').AsString := 'A';
          FCdsDiagramas.Post;
        end;
      end;
    end;

    if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
  end
  else
  begin
    Application.MessageBox(
      'Não foi possível estabelecer conexão como banco de dados. Verifique os dados de conexão.',
      'Atenção', MB_ICONEXCLAMATION + MB_OK);
  end;
end;

procedure TDiagramaManager.OpenGenericContainer(Id: string; container: TObject);
begin
  if container is TEntityContainer then
    OpenEntityContainer(Id)
  else if container is TFrameConsultaDados then
    OpenAmostraContainer(Id);
end;

procedure TDiagramaManager.RemoveContainerDaListaCarregados(Id: string);
var
  index: integer;
begin

  index := FListEntityContainerCarregados.IndexOf(Id);
  if index > -1 then
    // remove da lista de objetos carregados
    FListEntityContainerCarregados.Delete(index);

  // volta a tela anteior como visivel
  if FContainerAnteior <> nil then
  begin
    OpenGenericContainer(FListEntityContainerCarregados[
      FListEntityContainerCarregados.IndexOfObject(FContainerAnteior)], FContainerAnteior);
  end;

  if Assigned(FMenuItemParaDiagramasAbertos) then
  begin
    index := FListMenusDeDiagramasAbertos.IndexOf(Id);
    if index > -1 then
    begin
      // remove o menu
      FMenuItemParaDiagramasAbertos.Remove(FListMenusDeDiagramasAbertos.Data[index]);
      FListMenusDeDiagramasAbertos.Data[index].Free;
      // remove o menu da lista de menus
      FListMenusDeDiagramasAbertos.Delete(index);
    end;
  end;
end;

procedure TDiagramaManager.RemoverDiagrama(Id: string);
var
  idxOfDiagrama, idxOfMenu: integer;
begin
  if FCdsDiagramas.Locate('id', Id, []) then
  begin
    FCdsDiagramas.Edit;
    FCdsDiagramas.FieldByName('status').AsString := 'E';
    FCdsDiagramas.Post;

    FEntityContainerCorrente.EntityArea.Visible := False;
    FEntityContainerCorrente := nil;

    // remove o menu rápido
    idxOfDiagrama := FListMenusDeDiagramasAbertos.IndexOf(Id);
    idxOfMenu := FMenuItemParaDiagramasAbertos.IndexOf(
      FListMenusDeDiagramasAbertos.Data[idxOfDiagrama]);
    FMenuItemParaDiagramasAbertos.Remove(
      FListMenusDeDiagramasAbertos.Data[idxOfDiagrama]);
    FListMenusDeDiagramasAbertos.Data[idxOfMenu].Free;
    FListMenusDeDiagramasAbertos.Delete(idxOfDiagrama);

    if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
  end;
end;

procedure TDiagramaManager.RenderizarDiagrama(Id: string;
  entityContainer: TEntityContainer);
var
  diagrama: TDiagrama;
  //entidades: IXMLEntidadesType;
  entidade: TEntidade;
  //relacionamentos: IXMLRelacionamentosType;
  relacionamento: TRelacionamento;
  relationship: TEntityRelationship;
  i, k: integer;
begin
  // nao processar a posição das setas sem ter terminado de carregar o diagrama
  entityContainer.NaoRenderizarArrows := True;

  for i := 0 to AppFile.Diagramas.Count - 1 do
    if AppFile.Diagramas[i].Id = Id then
    begin
      diagrama := AppFile.Diagramas[i];
      // adiciona as entidades
      for k := 0 to diagrama.Entidades.Count - 1 do
      begin
        entidade := diagrama.Entidades[k];
        entityContainer.AddEntity(entidade.Owner, entidade.Tabela,
          entidade.Top, entidade.Left, entidade.TodosOsCampos);
      end;
      // reposiciona o relacionamento conforme salvo em disco
      for k := 0 to diagrama.Relacionamentos.Count - 1 do
      begin
        relacionamento := diagrama.Relacionamentos[k];
        relationship := entityContainer.FindRelacionamento(relacionamento.Owner,
          relacionamento.ConstraintName);
        if relationship <> nil then
        begin
          relationship.DistanciaLateral := relacionamento.DistanciaLateral;
          relationship.AplicarPosicaoCalculada;
        end;
      end;

      Break;
    end;
  entityContainer.NaoRenderizarArrows := False;
end;

procedure TDiagramaManager.RenomearDiagrama(Id: string);
var
  titulo: string;
  onde: integer;
begin
  if FCdsDiagramas.Locate('id', Id, []) then
  begin
    titulo := InputBox('Novo Diagrama', 'Digite o novo título do Diagrama', '');
    if titulo <> '' then
    begin
      FCdsDiagramas.Edit;
      FCdsDiagramas.FieldByName('titulo').AsString := titulo;
      FCdsDiagramas.Post;

      onde := FListEntityContainerCarregados.IndexOf(Id);
      if onde >= 0 then
        TEntityContainer(FListEntityContainerCarregados.Objects[onde]).Titulo := titulo;

      if Assigned(FOnMudancaEstadoModelo) then
        FOnMudancaEstadoModelo(Self);
    end;
  end;
end;

function TDiagramaManager.SaveModelo: boolean;
begin
  Result := False;
  if FOpenedFile <> '' then
  begin
    WriteFile;
    Result := True;
  end
  else if FSaveDialog.Execute then
  begin
    FOpenedFile := FSaveDialog.FileName;
    WriteFile;
    Result := True;
  end;

  if Assigned(FOnMudancaEstadoModelo) then
    FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.WriteFile;

  procedure WriteEntidades(diagramaId: string; diagramaDest: TDiagrama);
  var
    i, onde: integer;
    container: TEntityContainer;
    entity: TEntity;
    relationship: TEntityRelationship;
  begin
    // localiza o entity container do diagrama que se quer salvar
    onde := FListEntityContainerCarregados.IndexOf(diagramaId);
    if onde >= 0 then
    begin
      container := TEntityContainer(FListEntityContainerCarregados.Objects[onde]);
      // salva as entidades do container
      for i := 0 to container.ListEntity.Count - 1 do
      begin
        entity := TEntity(container.ListEntity.Objects[i]);

        diagramaDest.Entidades.Add(TEntidade.Create(
          TPanel(entity).Top, TPanel(entity).Left, entity.SchemaOwner,
          entity.NomeTabela, entity.ExibindoTodosOsCampos));
      end;

      // salva os relacionamentos do container
      for i := 0 to container.ListRelationship.Count - 1 do
      begin
        relationship := TEntityRelationship(container.ListRelationship.Objects[i]);
        diagramaDest.Relacionamentos.Add(TRelacionamento.Create(
          relationship.NomeCaminhoUsado, relationship.DistanciaLateral,
          relationship.SchemaOwner, relationship.ConstraintName));
      end;
    end;
  end;

var
  i: integer;
  diagrama: TDiagrama;
begin
  FCdsDiagramas.First;
  while not FCdsDiagramas.EOF do
  begin
    // se tem algo de novo no diagrama
    if FCdsDiagramas.FieldByName('status').AsString = 'N' then // novo
    begin
      diagrama := TDiagrama.Create(FCdsDiagramas.FieldByName('titulo').AsString,
        FCdsDiagramas.FieldByName('id').AsString);
      AppFile.Diagramas.Add(diagrama);
      WriteEntidades(diagrama.Id, diagrama);
      FCdsDiagramas.Edit;
      FCdsDiagramas.FieldByName('status').AsString := 'A';
      FCdsDiagramas.Post;
    end
    else if FCdsDiagramas.FieldByName('status').AsString = 'E' then // excluído
    begin
      for i := AppFile.Diagramas.Count - 1 downto 0 do
      begin
        if AppFile.Diagramas[i].Id = FCdsDiagramas.FieldByName('id').AsString then
        begin
          AppFile.Diagramas.Delete(i);
        end;
      end;
    end
    else if FCdsDiagramas.FieldByName('status').AsString = 'A' then // alterado
    begin
      for i := 0 to AppFile.Diagramas.Count - 1 do
      begin
        if AppFile.Diagramas[i].Id = FCdsDiagramas.FieldByName('id').AsString then
        begin
          diagrama := AppFile.Diagramas[i];
          diagrama.Titulo := FCdsDiagramas.FieldByName('titulo').AsString;

          while diagrama.Entidades.Count > 0 do
          begin
            diagrama.Entidades.Delete(0);
          end;
          while diagrama.Relacionamentos.Count > 0 do
          begin
            diagrama.Relacionamentos.Delete(0);
          end;

          WriteEntidades(diagrama.Id, diagrama);
        end;
      end;
    end;
    FCdsDiagramas.Next;
  end;

  if LowerCase(ExtractFileExt(FOpenedFile)) <> '.dbdata' then
    FOpenedFile := FOpenedFile + '.dbdata';

  AppFile.SaveFile(FOpenedFile);
end;

end.
