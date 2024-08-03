unit uDiagramaManager;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleotério
Data: 17/10/2013
}

interface

uses
  LCLIntf, LCLType, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, DB, BufDataset, SysUtils, Generics.Collections,
  uAppFile, uERNotationsCore, uFrameConsultaDados, uVariaveisGlobais; {fgl TFPGMapObject dicionário  }

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
    constructor Create(Id, Title: string;Diagram: TEntityContainer;Frame: TFrame;
      TypeOpenedFeature: TTypeOpenedFeature);
    property Id: string read FId;
    property Title: string read FTitle;
    property Diagram: TEntityContainer read FDiagram;
    property Frame: TFrame read FFrame;
    property MenuItem: TMenuItem read FMenuItem write FMenuItem;
    property TypeOpenedFeature: TTypeOpenedFeature read FTypeOpenedFeature;
  end;

  { TDiagramaManager }

  TDiagramaManager = class
  private
    FOpenedFeatures: specialize TObjectList<TOpenedFeature>;
    FOpenedFile: string;
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    FParentEntityContainer: TWinControl;
    FOnMudancaEstadoModelo: TNotifyEvent;
    FMenuItemParaDiagramasAbertos: TMenuItem;
    FCdsDiagramas: TBufDataSet;     // client data set com os nomes dos diagramas

    function GetCurrentDiagram: TEntityContainer;
    procedure WriteFile;
    procedure RenderizarDiagrama(Id: string; entityContainer: TEntityContainer);
    procedure FreeAllOpenedFeatures;
    procedure ClickMenuItemDiagAberto(Sender: TObject);
    function GetTemModeloParaSalvar: boolean;
    function GetIndexOfOpenedFeature(Id: string): Integer;
    procedure CreateMenuRapido(OpenedFeature: TOpenedFeature);
    function TestDBCnn: boolean;
  public
    constructor Create(ParentEntityContainer: TWinControl);
    destructor Destroy; override;
    procedure NovoModelo;
    procedure FecharModelo;
    procedure OpenModelo(DBDataFileName: string);
    function SaveModelo: boolean;
    procedure OpenEntityContainer(Id: string);
    procedure OpenAmostraContainer(OwnerPlusTabelaAsID: string);
    procedure NovoDiagrama;
    procedure RenomearDiagrama(Id: string);
    procedure RemoverDiagrama(Id: string);
    procedure FreeOpenedFeature(Id: string);
    procedure DoAllFeaturesInvisible;
    procedure DoAnotherFeatureVisible(DestinationIndex: Integer);
  published
    property CdsDiagramas: TBufDataSet read FCdsDiagramas write FCdsDiagramas;
    property OnMudancaEstadoModelo: TNotifyEvent read FOnMudancaEstadoModelo write FOnMudancaEstadoModelo;
    property TemModeloParaSalvar: boolean read GetTemModeloParaSalvar;
    property NomeModeloAberto: string read FOpenedFile;
    property MenuItemParaDiagramasAbertos: TMenuItem read FMenuItemParaDiagramasAbertos write FMenuItemParaDiagramasAbertos;
    property OpenedFeatures: specialize TObjectList<TOpenedFeature> read FOpenedFeatures write FOpenedFeatures;
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


  { TDiagramaManager }

procedure TDiagramaManager.FreeAllOpenedFeatures;
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
  FCdsDiagramas.First;
  while not FCdsDiagramas.EOF do
    FCdsDiagramas.Delete;

  // remove todos os menus de diagramas abertos
  if Assigned(FMenuItemParaDiagramasAbertos) then
  begin
    for i := FMenuItemParaDiagramasAbertos.Count -1 downto 0 do
    begin
      menu := FMenuItemParaDiagramasAbertos.Items[i];
      FMenuItemParaDiagramasAbertos.Remove(menu);
      menu.Free;
    end;
  end;
end;

function TDiagramaManager.GetTemModeloParaSalvar: boolean;
begin
  Result := Assigned(AppFile);
end;

function TDiagramaManager.GetIndexOfOpenedFeature(Id: string): Integer;
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

procedure TDiagramaManager.ClickMenuItemDiagAberto(Sender: TObject);
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

  if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
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

  FOpenedFeatures := specialize TObjectList<TOpenedFeature>.Create(True);
end;

destructor TDiagramaManager.Destroy;
begin
  FOpenDialog.Free;
  FSaveDialog.Free;
  FreeAllOpenedFeatures;
  FCdsDiagramas.Free;
  FOpenedFeatures.Free;
  inherited;
end;

procedure TDiagramaManager.FecharModelo;
begin
  FOpenedFile := '';
  FreeAndNil(AppFile);
  FreeAllOpenedFeatures;

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



procedure TDiagramaManager.OpenAmostraContainer(OwnerPlusTabelaAsID: string);
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
    frameAmostra := TFrameConsultaDados.Create(FParentEntityContainer);
    frameAmostra.Parent := FParentEntityContainer;
    frameAmostra.Name := ''; // da um nome qualquer;

    FOpenedFeatures.Add(TOpenedFeature.Create(OwnerPlusTabelaAsID, OwnerPlusTabelaAsID, nil, frameAmostra, tofFrame));
    CreateMenuRapido(FOpenedFeatures.Last);
    frameAmostra.ObterAmostra(OwnerPlusTabelaAsID);
  end;

  if Assigned(FOnMudancaEstadoModelo) then
    FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.CreateMenuRapido(OpenedFeature: TOpenedFeature);
var
  menu: TMenuItem;
begin
  if Assigned(FMenuItemParaDiagramasAbertos) then
  begin
    menu := TMenuItem.Create(FMenuItemParaDiagramasAbertos);
    menu.Caption := OpenedFeature.Title;
    menu.OnClick := @ClickMenuItemDiagAberto;
    FMenuItemParaDiagramasAbertos.Add(menu);
    OpenedFeature.MenuItem := menu;
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
    // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
    index := GetIndexOfOpenedFeature(Id);

    DoAllFeaturesInvisible;
    if index > -1 then
    begin
      FOpenedFeatures[index].Diagram.EntityArea.Visible := True;
    end
    else // se ele ainda não foi aberto, entao abre
    begin
      if FCdsDiagramas.Locate('id', Id, []) then
      begin
        container := TEntityContainer.Create(FParentEntityContainer);
        container.DiagramaId := FCdsDiagramas.FieldByName('id').AsString;
        container.Titulo := FCdsDiagramas.FieldByName('titulo').AsString;
        FOpenedFeatures.Add(TOpenedFeature.Create(container.DiagramaId, container.Titulo, container, nil, tofDiagram));
        CreateMenuRapido(FOpenedFeatures.Last);
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

procedure TDiagramaManager.FreeOpenedFeature(Id: string);
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

    if Assigned(FMenuItemParaDiagramasAbertos) then
    begin
      FMenuItemParaDiagramasAbertos.Remove(openedFeature.MenuItem);
      openedFeature.MenuItem.Free;
    end;

    openedFeature.Free;
    DoAnotherFeatureVisible(indexOfOpenedFeature -1);

    if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
  end;
end;

procedure TDiagramaManager.DoAllFeaturesInvisible;
var
  openedFeature: TOpenedFeature;
begin
  for openedFeature in FOpenedFeatures do
  begin
    if Assigned(openedFeature.Diagram) then openedFeature.Diagram.EntityArea.Visible := False;
    if Assigned(openedFeature.Frame) then openedFeature.Frame.Visible := False;
  end;
end;

procedure TDiagramaManager.DoAnotherFeatureVisible(DestinationIndex: Integer);
begin
  if FOpenedFeatures.Count > 0 then
  begin
    if DestinationIndex < 0 then
      DestinationIndex := 0;

    if FOpenedFeatures.Items[DestinationIndex].TypeOpenedFeature = tofDiagram then
      OpenEntityContainer(FOpenedFeatures.Items[DestinationIndex].Id)
    else if FOpenedFeatures.Items[DestinationIndex].TypeOpenedFeature = tofFrame then
      OpenAmostraContainer(FOpenedFeatures.Items[DestinationIndex].Id);
  end;
end;

procedure TDiagramaManager.RemoverDiagrama(Id: string);

begin
  if FCdsDiagramas.Locate('id', Id, []) then
  begin
    FCdsDiagramas.Edit;
    FCdsDiagramas.FieldByName('status').AsString := 'E';
    FCdsDiagramas.Post;

    FreeOpenedFeature(Id);
  end;
end;

procedure TDiagramaManager.RenderizarDiagrama(Id: string;
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

      {onde := FListEntityContainerCarregados.IndexOf(Id);
      if onde >= 0 then
        TEntityContainer(FListEntityContainerCarregados.Objects[onde]).Titulo := titulo;}

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

function TDiagramaManager.GetCurrentDiagram: TEntityContainer;
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
