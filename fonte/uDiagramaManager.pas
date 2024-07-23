unit uDiagramaManager;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 17/10/2013
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, StdCtrls, Buttons, DB, BufDataset,
  uDigsERFile, uERNotationsCore, uFrameConsultaDados;

type
  TDiagramaManager = class
    private
      FNomeModeloAberto: string;
      FAppFileFormat: TAppFileFormat;
      FListEntityContainerCarregados: TStringList;
      FListMenusDeDiagramasAbertos: TStringList;
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
      //
      procedure WriteFile;
      procedure RenderizarDiagrama(Id: string; entityContainer: TEntityContainer);
      procedure FreeAllEntityContainer;
      //
      procedure ClickMenuItemDiagAberto(Sender: TObject);
      procedure OpenGenericContainer(Id: string; container: TObject);
      // acesso a propriedades
      function GetTemModeloParaSalvar: Boolean;
    public
      constructor Create(ParentEntityContainer: TWinControl);
      destructor Destroy; override;
      procedure NovoModelo;
      procedure FecharModelo;
      procedure OpenModelo(digsERFile: string);
      function SaveModelo: Boolean;
      procedure PrepareOpenContainer;
      procedure OpenEntityContainer(Id: string);
      procedure CreateMenuRapido(diagramaId, captionMenuItem: string);
      procedure OpenAmostraContainer(OwnerTabela: string);
      procedure RemoverDiagrama(Id: string);
      procedure RenomearDiagrama(Id: string);
      procedure NovoDiagrama;
      procedure RemoveContainerDaListaCarregados(Id: string);
    published
      property CdsDiagramas: TBufDataSet read FCdsDiagramas write FCdsDiagramas;
      property EntityContainerCorrente: TEntityContainer read FEntityContainerCorrente;
      property AmostraDadosCorrente: TFrameConsultaDados read FAmostraDadosCorrente;
      property OnMudancaEstadoModelo: TNotifyEvent read FOnMudancaEstadoModelo write FOnMudancaEstadoModelo;
      property TemModeloParaSalvar: Boolean read GetTemModeloParaSalvar;
      property NomeModeloAberto: string read FNomeModeloAberto;
      property MenuItemParaDiagramasAbertos: TMenuItem read FMenuItemParaDiagramasAbertos write FMenuItemParaDiagramasAbertos;
  end;

implementation

uses uPrincipal, uConnection;


{ TDiagramaManager }

procedure TDiagramaManager.FreeAllEntityContainer;
var
  i: Integer;
begin
  // remove todos os entity containers da memória
  for i := FListEntityContainerCarregados.Count - 1 downto 0 do
    TObject(FListEntityContainerCarregados.Objects[i]).Free;
  FListEntityContainerCarregados.Clear;

  // limpa o client dataset axiliar
  FCdsDiagramas.First;
  while not FCdsDiagramas.Eof do
    FCdsDiagramas.Delete;

  // remove o entity container corrente
  FEntityContainerCorrente := nil;

  // remove todos os menus de diagramas abertos
  for i := FListMenusDeDiagramasAbertos.Count -1 downto 0 do
  begin
    if Assigned(FMenuItemParaDiagramasAbertos) then
      FMenuItemParaDiagramasAbertos.Remove(TMenuItem(FListMenusDeDiagramasAbertos.Objects[i]));

    TMenuItem(FListMenusDeDiagramasAbertos.Objects[i]).Free;
    FListMenusDeDiagramasAbertos.Delete(i);
  end;
end;

function TDiagramaManager.GetTemModeloParaSalvar: Boolean;
begin
  Result := Assigned(FAppFileFormat);
end;

procedure TDiagramaManager.ClickMenuItemDiagAberto(Sender: TObject);
var
  index: Integer;
  id: string;
  container: TObject;
begin

  index := FListMenusDeDiagramasAbertos.IndexOfObject(Sender);
  if index > -1 then
  begin
    id :=  FListMenusDeDiagramasAbertos[index];
    index := FListEntityContainerCarregados.IndexOf(id);
    container := FListEntityContainerCarregados.Objects[index];

    OpenGenericContainer(FListMenusDeDiagramasAbertos[index], container);
  end;
end;

constructor TDiagramaManager.Create(ParentEntityContainer: TWinControl);
begin
  inherited Create;
  FParentEntityContainer := ParentEntityContainer;
  FOpenDialog := TOpenDialog.Create(nil);
  FOpenDialog.Filter := 'Arquivos do DigsER (*.dger)|*.DGER';

  FSaveDialog := TSaveDialog.Create(nil);
  FSaveDialog.Filter := 'Arquivos do DigsER (*.dger)|*.DGER';

  FCdsDiagramas := TBufDataSet.Create(nil);
  FCdsDiagramas.FieldDefs.Add('titulo', ftString, 300, false);
  FCdsDiagramas.FieldDefs.Add('id', ftString, 64, False);
  FCdsDiagramas.IndexDefs.Add('idxTitulo','titulo', []);
  // status: N -> Novo, A -> Alterado, E -> Excluído, 0 - Carregado do XML
  FCdsDiagramas.FieldDefs.Add('status', ftString, 1, false);
  FCdsDiagramas.CreateDataSet;
  FCdsDiagramas.IndexFieldNames := 'titulo';

  FListEntityContainerCarregados := TStringList.Create;
  FListMenusDeDiagramasAbertos := TStringList.Create;
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
  FNomeModeloAberto := '';
  FreeAndNil(FAppFileFormat);
  FreeAllEntityContainer;

  if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.NovoDiagrama;
var
  titulo: string;
  //
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
    Application.MessageBox('Nome do diagrama não informado!', 'ATENÇÃO', MB_OK 
      + MB_ICONSTOP);
      
end;

procedure TDiagramaManager.NovoModelo;
begin
  FAppFileFormat := { aqui Newdiagramas} nil;

  if Assigned(FOnMudancaEstadoModelo) then
      FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.OpenModelo(digsERFile: string);
var
  i: Integer;
  diagrama: TDiagrama;
begin
  // se o nome do arquivo não vier como parâmetro, então carregar pela janela
  if digsERFile = '' then
  begin
    if FOpenDialog.Execute then
      FNomeModeloAberto := FOpenDialog.FileName;
  end
  else
    FNomeModeloAberto := digsERFile;

  if FNomeModeloAberto <> '' then
  begin
    FAppFileFormat := TAppFileFormat.Create();
    FAppFileFormat.OpenFile(FNomeModeloAberto);
    for i := 0 to FAppFileFormat.Diagramas.Count - 1 do
    begin
      diagrama := FAppFileFormat.Diagramas[i];
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
  index: Integer;
begin
  PrepareOpenContainer;

  // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
  index := FListEntityContainerCarregados.IndexOf(OwnerTabela);

  if index > -1 then
  begin
    TFrameConsultaDados(FListEntityContainerCarregados.Objects[index]).Visible := True;
    FAmostraDadosCorrente := TFrameConsultaDados(FListEntityContainerCarregados.Objects[index]);
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
    menu.OnClick := ClickMenuItemDiagAberto;
    FListMenusDeDiagramasAbertos.AddObject(diagramaId, menu);
    FMenuItemParaDiagramasAbertos.Add(menu);
  end;
end;

procedure TDiagramaManager.OpenEntityContainer(Id: string);
var
  container: TEntityContainer;
  index: Integer;
begin
  PrepareOpenContainer;

  // verifica se ele já foi aberto, se sim, coloca em tela simplesmente
  index := FListEntityContainerCarregados.IndexOf(Id);

  if index > -1 then
  begin
    TEntityContainer(FListEntityContainerCarregados.Objects[index]).EntityArea.Visible := True;
    FEntityContainerCorrente := TEntityContainer(FListEntityContainerCarregados.Objects[index]);
  end
  else
  // se ele ainda não foi aberto, entao abre
  begin
    // testa se a conexão está funcionando ou dá pau no ato
    TConexao.GetConexao;
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
  index: Integer;
begin

  index := FListEntityContainerCarregados.IndexOf(Id);
  if index > -1 then
    // remove da lista de objetos carregados
    FListEntityContainerCarregados.Delete(index);

  // volta a tela anteior como visivel
  if FContainerAnteior <> nil then
  begin
    OpenGenericContainer(FListEntityContainerCarregados[FListEntityContainerCarregados.IndexOfObject(FContainerAnteior)] ,FContainerAnteior);

  end;

  if Assigned(FMenuItemParaDiagramasAbertos) then
  begin
    index := FListMenusDeDiagramasAbertos.IndexOf(Id);
    if index > -1 then
    begin
      // remove o menu
      FMenuItemParaDiagramasAbertos.Remove(TMenuItem(FListMenusDeDiagramasAbertos.Objects[index]));
      FListMenusDeDiagramasAbertos.Objects[index].Free;
      // remove o menu da lista de menus
      FListMenusDeDiagramasAbertos.Delete(index);
    end;
  end;
end;

procedure TDiagramaManager.RemoverDiagrama(Id: string);
begin
  if FCdsDiagramas.Locate('id', Id, []) then
  begin
    FCdsDiagramas.Edit;
    FCdsDiagramas.FieldByName('status').AsString := 'E';
    FCdsDiagramas.Post;

    FEntityContainerCorrente.EntityArea.Visible := False;
    FEntityContainerCorrente := nil;

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
  i, k: Integer;
begin
  // nao processar a posição das setas sem ter terminado de carregar o diagrama
  entityContainer.NaoRenderizarArrows := True;
  
    for i := 0 to FAppFileFormat.Diagramas.Count - 1 do
    if FAppFileFormat.Diagramas[i].Id = Id then
    begin
      diagrama := FAppFileFormat.Diagramas[i];
      // adiciona as entidades
      for k := 0 to diagrama.Entidades.Count - 1 do
      begin
        entidade := diagrama.Entidades[k];
        entityContainer.AddEntity(entidade.Owner, entidade.Tabela, entidade.Top, entidade.Left, entidade.TodosOsCampos);
      end;
      // reposiciona o relacionamento conforme salvo em disco
      for k := 0 to diagrama.Relacionamentos.Count - 1 do
      begin
        relacionamento := diagrama.Relacionamentos[k];
        relationship := entityContainer.FindRelacionamento(relacionamento.Owner, relacionamento.ConstraintName);
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
  onde: Integer;
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

function TDiagramaManager.SaveModelo: Boolean;
begin
  Result := False;
  if FNomeModeloAberto <> '' then
  begin
    WriteFile;
    Result := True;
  end
  else if FSaveDialog.Execute then
  begin
    FNomeModeloAberto := FSaveDialog.FileName;
    WriteFile;
    Result := True;
  end;

  if Assigned(FOnMudancaEstadoModelo) then
    FOnMudancaEstadoModelo(Self);
end;

procedure TDiagramaManager.WriteFile;
(*var
  diagrama: IXMLDiagramaType;
  entidades: IXMLEntidadesType;
  entidade: IXMLEntidadeType;
  relacionamentos: IXMLRelacionamentosType;
  relacionamento: IXMLRelacionamentoType;
  i: Integer;
  entity: TEntity;
  relationship: TEntityRelationship;

  procedure WriteEntidades(diagramaId: string);
  var
    i, onde: Integer;
    container: TEntityContainer;
  begin
    // localiza o entity container do diagrama que se quer salvar
    onde := FListEntityContainerCarregados.IndexOf(diagramaId);
    if onde >= 0 then
    begin
      container := TEntityContainer(FListEntityContainerCarregados.Objects[onde]);
      // salva as entidades do container
      entidades := diagrama.Entidades;
      for i := 0 to container.ListEntity.Count - 1 do
      begin
        entidade := entidades.Add;
        entity := TEntity(container.ListEntity.Objects[i]);
        entidade.Top := TPanel(entity).Top;
        entidade.Left := TPanel(entity).Left;
        entidade.Owner := entity.SchemaOwner;
        entidade.Tabela := entity.NomeTabela;
        entidade.TodosOsCampos := entity.ExibindoTodosOsCampos;
      end;
      // salva os relacionamentos do container
      relacionamentos := diagrama.Relacionamentos;
      for i := 0 to container.ListRelationship.Count - 1 do
      begin
        relationship := TEntityRelationship(container.ListRelationship.Objects[i]);
        relacionamento := relacionamentos.Add;
        relacionamento.NomeCaminho := relationship.NomeCaminhoUsado;
        relacionamento.DistanciaLateral := relationship.DistanciaLateral;
        relacionamento.Onwer := relationship.SchemaOwner;
        relacionamento.ConstraintName := relationship.ConstraintName;
      end;
    end;
  end;

begin
  FFileContent.Versao := '1.0';

  FCdsDiagramas.First;
  while not FCdsDiagramas.Eof do
  begin
    // se tem algo de novo no diagrama
    if FCdsDiagramas.FieldByName('status').AsString = 'N' then // novo
    begin
      diagrama := FFileContent.Add;
      diagrama.Titulo := FCdsDiagramas.FieldByName('titulo').AsString;
      diagrama.Id := FCdsDiagramas.FieldByName('id').AsString;
      WriteEntidades(diagrama.Id);
      FCdsDiagramas.Edit;
      FCdsDiagramas.FieldByName('status').AsString := 'A';
      FCdsDiagramas.Post;
    end
    else if FCdsDiagramas.FieldByName('status').AsString = 'E' then // excluído
    begin
      {aqui for i := FFileContent.Count - 1 downto 0 do
      begin
        if FFileContent.Diagrama[i].Id = FCdsDiagramas.FieldByName('id').AsString then
          FFileContent.Delete(i);
      end;  }
    end
    else if FCdsDiagramas.FieldByName('status').AsString = 'A' then // alterado
    begin
      {aqui for i := 0 to FFileContent.Count - 1 do
      begin
        if FFileContent.Diagrama[i].Id = FCdsDiagramas.FieldByName('id').AsString then
        begin
          diagrama := FFileContent.Diagrama[i];
          diagrama.Titulo := FCdsDiagramas.FieldByName('titulo').AsString;
          while diagrama.Entidades.Count > 0 do
            diagrama.Entidades.Delete(0);
          while diagrama.Relacionamentos.Count > 0 do
            diagrama.Relacionamentos.Delete(0);

          WriteEntidades(diagrama.Id);
        end;
      end;  }
    end;
    FCdsDiagramas.Next;
  end;

  if LowerCase(ExtractFileExt(FNomeModeloAberto)) <> '.dger' then
    FNomeModeloAberto := FNomeModeloAberto + '.dger';

  {aqui FFileContent.OwnerDocument.SaveToFile(FNomeModeloAberto);}
end;  *) begin end;

end.
