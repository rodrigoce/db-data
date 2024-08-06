unit uFeaturesHandler;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleotério
Data: 17/10/2013
}

interface

uses
  LCLIntf, LCLType, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, DB, BufDataset, SysUtils, Generics.Collections,
  uAppFile, uERNotationsCore, uFrameConsultaDados, uVariaveisGlobais; {fgl TFPGMapObject dicionário}

type

  { TOpenedFeature }
  TTypeOpenedFeature = (tofDiagram, tofFrame);

  TOpenedFeature = class
  private
    FId: string;
    FTitle: string;
    FDiagram: TEntityContainer;
    FFrame: TFrame;
    FMenuItem: TMenuItem;
    FTypeOpenedFeature: TTypeOpenedFeature;
  public
    constructor Create(Id, Title: string; Diagram: TEntityContainer; Frame: TFrame; TypeOpenedFeature: TTypeOpenedFeature);
    property Id: string read FId;
    property Title: string read FTitle;
    property Diagram: TEntityContainer read FDiagram;
    property Frame: TFrame read FFrame;
    property MenuItem: TMenuItem read FMenuItem write FMenuItem;
    property TypeOpenedFeature: TTypeOpenedFeature read FTypeOpenedFeature;
  end;

  { TFeaturesHandler }

  TFeaturesHandler = class
  private
    FOpenedFeatures: specialize TObjectList<TOpenedFeature>;
    FOpenedFile: string;
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    FParentWinControl: TWinControl;
    FOnChangeFeatureState: TNotifyEvent;
    FMenuItemOpenedFeatures: TMenuItem;
    FBuffDiagrams: TBufDataSet;     // buff data set com os nomes dos diagramas

    function GetCurrentDiagram: TEntityContainer;
    procedure WriteFile;
    procedure RenderDiagram(Id: string; entityContainer: TEntityContainer);
    procedure FreeAllOpenedFeatures;
    procedure OpenedFeatureMenuItemClick(Sender: TObject);
    function GetHasFileToSave: boolean;
    function GetIndexOfOpenedFeature(Id: string): Integer;
    procedure CreateOpenedFeatureMenu(OpenedFeature: TOpenedFeature);
    function TestDBCnn: boolean;
    procedure DoAllFeaturesInvisible;
    procedure DoAnotherFeatureVisible(DestinationIndex: Integer);
  public
    constructor Create(ParentWinControl: TWinControl);
    destructor Destroy; override;
    procedure NewFile;
    procedure CloseFile;
    procedure OpenFile(DBDataFileName: string);
    function SaveFile: boolean;
    procedure OpenEntityContainer(Id: string);
    procedure OpenQueryContainer(OwnerPlusTabelaAsID: string);
    procedure NewDiagramER;
    procedure RenameDiagramER(Id: string);
    procedure RemoveDiagramER(Id: string);
    procedure FreeOpenedFeature(Id: string);
  published
    property BuffDiagrams: TBufDataSet read FBuffDiagrams write FBuffDiagrams;
    property OnChangeFeatureState: TNotifyEvent read FOnChangeFeatureState write FOnChangeFeatureState;
    property HasFileToSave: boolean read GetHasFileToSave;
    property OpenedFile: string read FOpenedFile;
    property MenuItemOpenedFeatures: TMenuItem read FMenuItemOpenedFeatures write FMenuItemOpenedFeatures;
    property CurrentDiagram: TEntityContainer read GetCurrentDiagram;
  end;

implementation

uses uConnection;

{ TOpenedFeature }

constructor TOpenedFeature.Create(Id, Title: string; Diagram: TEntityContainer; Frame: TFrame; TypeOpenedFeature: TTypeOpenedFeature);
begin
  inherited Create;
  FId := Id;
  FTitle := Title;
  FDiagram := Diagram;
  FFrame := Frame;
  FTypeOpenedFeature := TypeOpenedFeature;
end;


  { TFeaturesHandler }

procedure TFeaturesHandler.FreeAllOpenedFeatures;
var
  i: integer;
  menu: TMenuItem;
begin
  // remove todos os entity containers da memória
  for i := FOpenedFeatures.Count - 1 downto 0 do
  begin
    if Assigned(FOpenedFeatures[i].Diagram) then FOpenedFeatures[i].Diagram.Free;
    if Assigned(FOpenedFeatures[i].Frame) then FOpenedFeatures[i].Frame.Free;
  end;
  FOpenedFeatures.Clear; // automatic free at items

  // limpa o client dataset axiliar
  FBuffDiagrams.First;
  while not FBuffDiagrams.EOF do
    FBuffDiagrams.Delete;

  // remove todos os menus de diagramas abertos
  if Assigned(FMenuItemOpenedFeatures) then
  begin
    for i := FMenuItemOpenedFeatures.Count -1 downto 0 do
    begin
      menu := FMenuItemOpenedFeatures.Items[i];
      FMenuItemOpenedFeatures.Remove(menu);
      menu.Free;
    end;
  end;
end;

function TFeaturesHandler.GetHasFileToSave: boolean;
begin
  Result := Assigned(AppFile);
end;

function TFeaturesHandler.GetIndexOfOpenedFeature(Id: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FOpenedFeatures.Count -1 do
  begin
    if FOpenedFeatures[i].Id = Id then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TFeaturesHandler.OpenedFeatureMenuItemClick(Sender: TObject);
var
  i: Integer;
begin
  DoAllFeaturesInvisible;
  for i := 0 to FOpenedFeatures.Count -1 do
  begin
    if FOpenedFeatures[i].MenuItem = (Sender as TMenuItem) then
    begin
      if FOpenedFeatures[i].FTypeOpenedFeature = tofDiagram then
        FOpenedFeatures[i].Diagram.EntityArea.Visible := True
      else if FOpenedFeatures[i].FTypeOpenedFeature = tofFrame then
        FOpenedFeatures[i].Frame.Visible := True;
    end;
  end;

  if Assigned(FOnChangeFeatureState) then
      FOnChangeFeatureState(Self);
end;

constructor TFeaturesHandler.Create(ParentWinControl: TWinControl);
begin
  inherited Create;
  FParentWinControl := ParentWinControl;
  FOpenDialog := TOpenDialog.Create(nil);
  FOpenDialog.Filter := 'Arquivos do DB-Data (*.dbdata)|*.DBDATA';

  FSaveDialog := TSaveDialog.Create(nil);
  FSaveDialog.Filter := 'Arquivos do DB-Data (*.dbdata)|*.DBDATA';

  FBuffDiagrams := TBufDataSet.Create(nil);
  FBuffDiagrams.FieldDefs.Add('titulo', ftString, 300, False);
  FBuffDiagrams.FieldDefs.Add('id', ftString, 64, False);
  FBuffDiagrams.IndexDefs.Add('idxTitulo', 'titulo', []);
  // status: N -> Novo, A -> Alterado, E -> Excluído, 0 - Carregado do XML
  FBuffDiagrams.FieldDefs.Add('status', ftString, 1, False);
  FBuffDiagrams.CreateDataSet;
  FBuffDiagrams.IndexFieldNames := 'titulo';

  FOpenedFeatures := specialize TObjectList<TOpenedFeature>.Create(True);
end;

destructor TFeaturesHandler.Destroy;
begin
  FOpenDialog.Free;
  FSaveDialog.Free;
  FreeAllOpenedFeatures;
  FBuffDiagrams.Free;
  FOpenedFeatures.Free;
  inherited;
end;

procedure TFeaturesHandler.CloseFile;
begin
  FOpenedFile := '';
  FreeAndNil(AppFile);
  FreeAllOpenedFeatures;

  if Assigned(FOnChangeFeatureState) then
    FOnChangeFeatureState(Self);
end;

procedure TFeaturesHandler.NewDiagramER;
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

    FBuffDiagrams.Append;
    FBuffDiagrams.FieldByName('id').AsString := GuidToString(Uid);
    FBuffDiagrams.FieldByName('titulo').AsString := titulo;
    FBuffDiagrams.FieldByName('status').AsString := 'N'; // novo
    FBuffDiagrams.Post;

    OpenEntityContainer(FBuffDiagrams.FieldByName('id').AsString);

    if Assigned(FOnChangeFeatureState) then
      FOnChangeFeatureState(Self);
  end
  else
    Application.MessageBox('Nome do diagrama não informado!', 'ATENÇÃO',
      MB_OK + MB_ICONSTOP);

end;

procedure TFeaturesHandler.NewFile;
begin
  AppFile := TAppFile.Create;

  if Assigned(FOnChangeFeatureState) then
    FOnChangeFeatureState(Self);
end;

procedure TFeaturesHandler.OpenFile(DBDataFileName: string);
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
      FBuffDiagrams.Append;
      FBuffDiagrams.FieldByName('id').AsString := diagrama.Id;
      FBuffDiagrams.FieldByName('titulo').AsString := diagrama.Titulo;
      FBuffDiagrams.FieldByName('status').AsString := '0';
      FBuffDiagrams.Post;
    end;
    FBuffDiagrams.First;

    if Assigned(FOnChangeFeatureState) then
      FOnChangeFeatureState(Self);
  end;

end;



procedure TFeaturesHandler.OpenQueryContainer(OwnerPlusTabelaAsID: string);
var
  frameAmostra: TFrameConsultaDados;
  indexOfOpenedFeature: integer;
begin
  // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
  indexOfOpenedFeature := GetIndexOfOpenedFeature(OwnerPlusTabelaAsID);

  DoAllFeaturesInvisible;
  if indexOfOpenedFeature > -1 then
    FOpenedFeatures[indexOfOpenedFeature].Frame.Visible := True
  else
  begin
    frameAmostra := TFrameConsultaDados.Create(FParentWinControl);
    frameAmostra.Parent := FParentWinControl;
    frameAmostra.Name := ''; // da um nome qualquer;

    FOpenedFeatures.Add(TOpenedFeature.Create(OwnerPlusTabelaAsID, OwnerPlusTabelaAsID, nil, frameAmostra, tofFrame));
    CreateOpenedFeatureMenu(FOpenedFeatures.Last);
    frameAmostra.ObterAmostra(OwnerPlusTabelaAsID);
  end;

  if Assigned(FOnChangeFeatureState) then
    FOnChangeFeatureState(Self);
end;

procedure TFeaturesHandler.CreateOpenedFeatureMenu(OpenedFeature: TOpenedFeature);
var
  menu: TMenuItem;
begin
  if Assigned(FMenuItemOpenedFeatures) then
  begin
    menu := TMenuItem.Create(FMenuItemOpenedFeatures);
    menu.Caption := OpenedFeature.Title;
    menu.OnClick := @OpenedFeatureMenuItemClick;
    FMenuItemOpenedFeatures.Add(menu);
    OpenedFeature.MenuItem := menu;
  end;
end;

function TFeaturesHandler.TestDBCnn: boolean;
begin
  try
    TConexao.FecharConexao;
    TConexao.GetConexao;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TFeaturesHandler.OpenEntityContainer(Id: string);
var
  container: TEntityContainer;
  index: integer;
begin
  if TestDBCnn then
  begin
    // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
    index := GetIndexOfOpenedFeature(Id);

    DoAllFeaturesInvisible;
    if index > -1 then
    begin
      FOpenedFeatures[index].Diagram.EntityArea.Visible := True;
    end
    else // se ele ainda não foi aberto, entao abre
    begin
      if FBuffDiagrams.Locate('id', Id, []) then
      begin
        container := TEntityContainer.Create(FParentWinControl);
        container.DiagramaId := FBuffDiagrams.FieldByName('id').AsString;
        container.Titulo := FBuffDiagrams.FieldByName('titulo').AsString;
        FOpenedFeatures.Add(TOpenedFeature.Create(container.DiagramaId, container.Titulo, container, nil, tofDiagram));
        CreateOpenedFeatureMenu(FOpenedFeatures.Last);
        // só abre os diagramas já carregados do disco (0) e muda o status para alterado (A)
        // pois todos os demais entrarão na lógica do blodo dos diagramas já em memória
        if FBuffDiagrams.FieldByName('status').AsString = '0' then
        begin
          RenderDiagram(Id, container);
          FBuffDiagrams.Edit;
          FBuffDiagrams.FieldByName('status').AsString := 'A';
          FBuffDiagrams.Post;
        end;
      end;
    end;

    if Assigned(FOnChangeFeatureState) then
      FOnChangeFeatureState(Self);
  end
  else
  begin
    Application.MessageBox(
      'Não foi possível estabelecer conexão como banco de dados. Verifique os dados de conexão.',
      'Atenção', MB_ICONEXCLAMATION + MB_OK);
  end;
end;

procedure TFeaturesHandler.FreeOpenedFeature(Id: string);
var
  indexOfOpenedFeature: integer;
  openedFeature: TOpenedFeature;
begin
  indexOfOpenedFeature := GetIndexOfOpenedFeature(Id);
  if indexOfOpenedFeature > -1 then
  begin
    openedFeature := FOpenedFeatures.Extract(FOpenedFeatures.Items[indexOfOpenedFeature]);
    if Assigned(openedFeature.Diagram) then openedFeature.Diagram.Free;
    if Assigned(openedFeature.Frame) then openedFeature.Frame.Free;

    if Assigned(FMenuItemOpenedFeatures) then
    begin
      FMenuItemOpenedFeatures.Remove(openedFeature.MenuItem);
      openedFeature.MenuItem.Free;
    end;

    openedFeature.Free;
    DoAnotherFeatureVisible(indexOfOpenedFeature -1);

    if Assigned(FOnChangeFeatureState) then
      FOnChangeFeatureState(Self);
  end;
end;

procedure TFeaturesHandler.DoAllFeaturesInvisible;
var
  openedFeature: TOpenedFeature;
begin
  for openedFeature in FOpenedFeatures do
  begin
    if Assigned(openedFeature.Diagram) then openedFeature.Diagram.EntityArea.Visible := False;
    if Assigned(openedFeature.Frame) then openedFeature.Frame.Visible := False;
  end;
end;

procedure TFeaturesHandler.DoAnotherFeatureVisible(DestinationIndex: Integer);
begin
  if FOpenedFeatures.Count > 0 then
  begin
    if DestinationIndex < 0 then
      DestinationIndex := 0;

    if FOpenedFeatures.Items[DestinationIndex].TypeOpenedFeature = tofDiagram then
      OpenEntityContainer(FOpenedFeatures.Items[DestinationIndex].Id)
    else if FOpenedFeatures.Items[DestinationIndex].TypeOpenedFeature = tofFrame then
      OpenQueryContainer(FOpenedFeatures.Items[DestinationIndex].Id);
  end;
end;

procedure TFeaturesHandler.RemoveDiagramER(Id: string);

begin
  if FBuffDiagrams.Locate('id', Id, []) then
  begin
    FBuffDiagrams.Edit;
    FBuffDiagrams.FieldByName('status').AsString := 'E';
    FBuffDiagrams.Post;

    FreeOpenedFeature(Id);
  end;
end;

procedure TFeaturesHandler.RenderDiagram(Id: string;
  entityContainer: TEntityContainer);
var
  diagrama: TDiagrama;
  entidade: TEntidade;
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

procedure TFeaturesHandler.RenameDiagramER(Id: string);
var
  titulo: string;
  onde: integer;
begin
  if FBuffDiagrams.Locate('id', Id, []) then
  begin
    titulo := InputBox('Novo Diagrama', 'Digite o novo título do Diagrama', '');
    if titulo <> '' then
    begin
      FBuffDiagrams.Edit;
      FBuffDiagrams.FieldByName('titulo').AsString := titulo;
      FBuffDiagrams.Post;

      {onde := FListEntityContainerCarregados.IndexOf(Id);
      if onde >= 0 then
        TEntityContainer(FListEntityContainerCarregados.Objects[onde]).Titulo := titulo;}

      if Assigned(FOnChangeFeatureState) then
        FOnChangeFeatureState(Self);
    end;
  end;
end;

function TFeaturesHandler.SaveFile: boolean;
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

  if Assigned(FOnChangeFeatureState) then
    FOnChangeFeatureState(Self);
end;

procedure TFeaturesHandler.WriteFile;

  procedure WriteEntidades(diagramaId: string; diagramaDest: TDiagrama);
  var
    i, indexOpenedFeature: integer;
    container: TEntityContainer;
    entity: TEntity;
    relationship: TEntityRelationship;
  begin
    // localiza o entity container do diagrama que se quer salvar
    indexOpenedFeature := GetIndexOfOpenedFeature(diagramaId);
    if indexOpenedFeature >= 0 then
    begin
      container := FOpenedFeatures[indexOpenedFeature].Diagram;
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
  FBuffDiagrams.First;
  while not FBuffDiagrams.EOF do
  begin
    // se tem algo de novo no diagrama
    if FBuffDiagrams.FieldByName('status').AsString = 'N' then // novo
    begin
      diagrama := TDiagrama.Create(FBuffDiagrams.FieldByName('titulo').AsString,
        FBuffDiagrams.FieldByName('id').AsString);
      AppFile.Diagramas.Add(diagrama);
      WriteEntidades(diagrama.Id, diagrama);
      FBuffDiagrams.Edit;
      FBuffDiagrams.FieldByName('status').AsString := 'A';
      FBuffDiagrams.Post;
    end
    else if FBuffDiagrams.FieldByName('status').AsString = 'E' then // excluído
    begin
      for i := AppFile.Diagramas.Count - 1 downto 0 do
      begin
        if AppFile.Diagramas[i].Id = FBuffDiagrams.FieldByName('id').AsString then
        begin
          AppFile.Diagramas.Delete(i);
        end;
      end;
    end
    else if FBuffDiagrams.FieldByName('status').AsString = 'A' then // alterado
    begin
      for i := 0 to AppFile.Diagramas.Count - 1 do
      begin
        if AppFile.Diagramas[i].Id = FBuffDiagrams.FieldByName('id').AsString then
        begin
          diagrama := AppFile.Diagramas[i];
          diagrama.Titulo := FBuffDiagrams.FieldByName('titulo').AsString;

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
    FBuffDiagrams.Next;
  end;

  if LowerCase(ExtractFileExt(FOpenedFile)) <> '.dbdata' then
    FOpenedFile := FOpenedFile + '.dbdata';

  AppFile.SaveFile(FOpenedFile);
end;

function TFeaturesHandler.GetCurrentDiagram: TEntityContainer;
var
  openedFeature: TOpenedFeature;
begin
  Result := nil;
  for openedFeature in FOpenedFeatures do
  begin
    if (openedFeature.TypeOpenedFeature = tofDiagram) and (openedFeature.Diagram.EntityArea.Visible) then
    begin
      Result := openedFeature.Diagram;
      Break;
    end;
  end;
end;

end.
