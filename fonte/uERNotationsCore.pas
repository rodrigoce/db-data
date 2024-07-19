unit uERNotationsCore;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 12/02/2014
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, StdCtrls, Types, DB,
  Math;

// usado na TEntity
const
  LarguraInicialEntity = 40;

// usado na TCustomEntityColumn
const
  csLineHeight = 19;
  csLineMargin = 24;
  csConstraintWidth = 12;

// usado em TEntityRelationshipDragShape
const
  DistanciaLateralMinima = 2;

type

{ Tipos Genéricos }

  TTipoLinha = (TipoLinhaNomeTabela, TipoLinhaNomeColuna, TipoLinhaPK, TipoLinhaFK, TipoLinhaNaoChave);

  TPosicaoSeta = record
    Top: Integer;
    Left: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TSentidoSeta = (psCima, psBaixo, psDireita, psEsquerda);

  // usado em TRelationshipArrowPositioner
  TArrayOfTPoint = array of TPoint;
  TSegmentosSemInterseccao = record
    arrayVT: TArrayOfTPoint;
    arrayHR1: TArrayOfTPoint;
    arrayHR2: TArrayOfTPoint;
  end;

  { Forward Decls }

  // todas essas classes estão na mesma unit por fazerem
  // associação bi-direcional.
  // melhor e mais mais simples forma de evitar referência circular no delphi

  TEntityContainer = class;
  TEntity = class;
  TCustomEntityColumn = class;
  TEntityColumn = class;
  TEntityConstraint = class;
  TEntityRelationshipDragShape = class;
  TEntityRelationship = class;
  TRelationshipArrowPositioner = class;
  TPopMenuEntity = class;

  { Classes }

  TEntityContainer = class
    private
      FEntityArea: TScrollBox;
      FListEntity: TStringList;
      FListRelacionamento: TStringList;
      FDiagramaId: string;
      FTitulo: string;
      FPopMenuEntity: TPopMenuEntity;
      FRelationshipArrowPositioner: TRelationshipArrowPositioner;
      FNaoRenderizarArrows: Boolean;
      //
      procedure LigarRelacionamentos;
      function FindEntity(Owner, TableName: string): TEntity;
      procedure RegistrarRelacionamento(ConstraintOwner, ConstraintName: string; Relacionamento: TEntityRelationship);
    protected
      // acesso a propriedades
      procedure SetNaoRenderizarArrows(Value: Boolean);
    public
      constructor Create(OwnerParent: TWinControl);
      destructor Destroy; override;
      procedure AddEntity(owner, tabela: string; top, left: Integer; exibirTodosOsCampos: Boolean);
      procedure RemoveEntity(entity: TEntity);
      function FindRelacionamento(ConstraintOwner, ConstraintName: string): TEntityRelationship;
      procedure ScreenShot;
      procedure NotificarInicioArrasto;
      procedure NotificarFimArrasto;
      procedure NotificarDestaquePaisEFilhos(constraint: TEntityConstraint; Destacada: Boolean);
      procedure MoverTudo(direcao: TPoint);
    published
      property EntityArea: TScrollBox read FEntityArea;
      property ListEntity: TStringList read FListEntity;
      property ListRelationship: TStringList read FListRelacionamento;
      property DiagramaId: string read FDiagramaId write FDiagramaId;
      property Titulo: string read FTitulo write FTitulo;
      property PopMenuEntity: TPopMenuEntity read FPopMenuEntity;
      property NaoRenderizarArrows: Boolean read FNaoRenderizarArrows write SetNaoRenderizarArrows;
  end;

  // desenha o retangulo que representa cada entity (tabela)
  TEntity = class(TPanel)
    private
      FShapeBorda: TShape;
      FNomeTabela: string;
      FSchemaOnwer: string;
      FListColunasChave: TStringList;
      FListColunasNaoChave: TStringList;
      FListConstraints: TStringList;
      FListRelacionamentos: TStringList;
      FMaxWidthLinha: Word;
      FPrimaryKeyConstraintName: string;
      FEntityContainer: TEntityContainer;
      FExibindoTodosOsCampos: Boolean;
      procedure NotificarMudancaDeHeigth;
    public
      constructor Create(AOwner: TComponent; NomeTabela, Owner: string; EntityContainer: TEntityContainer; ExibirTodosOsCampos: Boolean); reintroduce;
      destructor Destroy; override;
      procedure NotificarNovaDimensao(NovaMaxWidthLinha: Word);
      // adiciona o nome da coluna na entidade
      function AddColunaChave(CaptionLinha: string; TipoLinha: TTipoLinha; Hint: string): TCustomEntityColumn;
      // adiciona a constraint da coluna na entidade
      function AddColunaConstraint(CaptionLinha: string; TipoLinha: TTipoLinha; Hint, ConstraintOwner, ConstraintName, ConstraintColumnName: string; PosicaoTop: Integer; ROwner, RConstraintName: string): TEntityConstraint;
      procedure AddColunaNaoChave(CaptionLinha: string; TipoLinha: TTipoLinha; Hint: string);
      procedure RemoverColunasNaoChave(EstaFechandoDiagrama: boolean);
      //procedure NotificarIntensificarCorConstraint(ConstratintName: string);
      //procedure NotificarEsmaecerCorConstraint(ConstratintName: string);
      procedure DestacarConstraint(constraint: TEntityConstraint; Destacada: Boolean);
      procedure NotificarArrasto;
      procedure RegistrarRelacionamento(Relacionamento: TEntityRelationship; SchemaOwner, ConstraintNameFK: string);
      procedure DesregistrarRelacionamento(Relacionamento: TEntityRelationship);
      function GetEntityColumnByName(NomeColuna: string): TCustomEntityColumn;
      procedure GetColumnsNamesPK(buffer: TStringList);
      procedure GetColumnsNamesFK(buffer: TStringList; Owner, ConstraintName: string);
    published
      property MaxWidthLinha: Word read FMaxWidthLinha;
      property SchemaOwner: string read FSchemaOnwer;
      property NomeTabela: string read FNomeTabela;
      property PrimaryKeyConstraintName: string read FPrimaryKeyConstraintName write FPrimaryKeyConstraintName;
      property ListConstraints: TStringList read FListConstraints;
      property ListColunasChave: TStringList read FListColunasChave;
      property EntityContainer: TEntityContainer read FEntityContainer write FEntityContainer;
      property ExibindoTodosOsCampos: Boolean read FExibindoTodosOsCampos write FExibindoTodosOsCampos;
  end;

  TCustomEntityColumn = class(TShape)
    private
      FCaption: TCaption;
      FTipoLinha: TTipoLinha;
      FCapturing: Boolean;
      FMouseDownSpot: TPoint;
      FCorDestacada: Boolean;
      FCountIntensificarCor: Word;

      procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure ShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    public
      constructor Create(AOwner: TComponent; Caption: string; TipoLinha: TTipoLinha; Hint: string); reintroduce;
      procedure IntensificarCor;
      procedure EsmaecerCor;
      procedure DeterminarDimensoes;
    protected
    // acesso a propriedades
      procedure SetCaption(Value: TCaption);
      procedure SetTipoLinha(Value: TTipoLinha);
      procedure SetCorDestacada(Value: Boolean);
      //
      procedure FKDoubleClick(Sender: TObject); virtual; abstract;
      procedure ChaveSingleClick(Sender: TObject); virtual; abstract;
      procedure Paint; override;
      procedure ShapeMouseEnter(Sender: TObject); virtual;
      procedure ShapeMouseLeave(Sender: TObject); virtual;
    published
      property Caption: TCaption read FCaption write SetCaption;
      property TipoLinha: TTipoLinha read FTipoLinha write SetTipoLinha;
      property CorDestacada: Boolean read FCorDestacada write SetCorDestacada;
  end;

  TEntityColumn = class(TCustomEntityColumn)
    private
    published
  end;

  TEntityConstraint = class(TCustomEntityColumn)
    private
      FConstraintOwner: string;
      FConstraintName: string;
      FConstraintColumnName: string;
      // esses dois atributos é preciso salvar no carregamento da entidade para
      // poder obter os 3 atributos debaixo
      FROwner: string;
      FRConstraintName: string;
      // 3 atributos debaixo são esses *__*
      FTabelaRelacionada: string;
      FOwnerTabelaRelacionada: string;
      FColunaRelacionada: string;
      FDestacadaPorClick: Boolean;
      //
    protected
      procedure ShapeMouseEnter(Sender: TObject); override;
      procedure ShapeMouseLeave(Sender: TObject); override;
      procedure FKDoubleClick(Sender: TObject); override;
      procedure ChaveSingleClick(Sender: TObject); override;
    published
      property ConstraintOwner: string read fConstraintOwner write FConstraintOwner;
      property ConstraintName: string read FConstraintName write FConstraintName;
      property ConstraintColumnName: string read FConstraintColumnName write FConstraintColumnName;
      property ROwner: string read FROwner write FROwner;
      property RConstraintName: string read FRConstraintName write FRConstraintName;
      property TabelaRelacionada: string read FTabelaRelacionada write FTabelaRelacionada;
      property OwnerTabelaRelacionada: string read FOwnerTabelaRelacionada write FOwnerTabelaRelacionada;
      property ColunaRelacionada: string read FColunaRelacionada write FColunaRelacionada;
  end;

  TEntityRelationshipDragShape = class(TShape)
    private
      FCapturing: Boolean;
      FMouseDownSpot: TPoint;
      FEntityRelationship: TEntityRelationship;
      procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure ShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      function NormalizarIncremento(X: Integer): Integer;
    public
      constructor Create(AOwner: TComponent; EntityRelationship: TEntityRelationship); reintroduce;
  end;

  TEntityRelationship = class
    private
      // são os shapes que desenham a seta
      FVT: TEntityRelationshipDragShape;
      FHR1: TShape;
      FHR2: TShape;
      // são as posições que os shapes devem assumir
      FPosiVT: TPosicaoSeta;
      FPosiHR1: TPosicaoSeta;
      FPosiHR2: TPosicaoSeta;
      // são os sentidos das setas em cada shape
      FSentidoVT: TSentidoSeta;
      FSentidoHR1: TSentidoSeta;
      FSentidoHR2: TSentidoSeta;
      //
      FDistanciaLateral: Integer;
      FPontoA: TPoint;
      FPontoB: TPoint;
      FPontoC: TPoint;
      FPontoD: TPoint;
      FEntityPai: TEntity;
      FLinhaNomeColunaPai: string;
      FEntityFilha: TEntity;
      FLinhaNomeColunaFilha: string;
      FNomeCaminhoUsado: string;
      FSchemaOwner: string;
      FConstraintname: string;
      FEntityContainer: TEntityContainer;
      FCorDestacada: Boolean;
      FHistoricoDeCor: TStringList;
      // eventos
      procedure ShapeMouseEnter(Sender: TObject);
      procedure ShapeMouseLeave(Sender: TObject);
      procedure Calcular(pAplicarPosicaoCalculada: Boolean);
      procedure IntensificarCor;
      procedure EsmaecerCor;
    protected
      // acesso de propriedades
      procedure SetDistanciaLateral(Value: Integer);
      procedure SetNomeCaminhoUsado(Value: string);
      procedure SetCorDestacada(Value: Boolean);
    public
      constructor Create(Owner: TWinControl; SchemaOwner, ConstraintName: string; EntityContainer: TEntityContainer);
      destructor Destroy; override;
      procedure SetarEntidadeFilha(entity: TEntity; NomeColuna: string);
      procedure SetarEntidadePai(entity: TEntity; NomeColuna: string);
      procedure NotificarArrasto;
      procedure AplicarPosicaoCalculada;
      procedure AplicarNovaCor(cor: TColor);
      procedure RemoverCor(cor: TColor);
      property PosiVT: TPosicaoSeta read FPosiVT write FPosiVT;
      property PosiHR1: TPosicaoSeta read FPosiHR1 write FPosiHR1;
      property PosiHR2: TPosicaoSeta read FPosiHR2 write FPosiHR2;
    published
      property SchemaOwner: string read FSchemaOwner write FSchemaOwner;
      property ConstraintName: string read FConstraintName write FConstraintName;
      property DistanciaLateral: Integer read FDistanciaLateral write SetDistanciaLateral;
      property NomeCaminhoUsado: string read FNomeCaminhoUsado write SetNomeCaminhoUsado;
      property SentidoVT: TSentidoSeta read FSentidoVT;
      property SentidoHR1: TSentidoSeta read FSentidoHR1;
      property SentidoHR2: TSentidoSeta read FSentidoHR2;
      property ShapeVT: TEntityRelationshipDragShape read FVT;
      property ShapeHR1: TShape read FHR1;
      property ShapeHR2: TShape read FHR2;
      property EntityContainer: TEntityContainer read FEntityContainer;
      property CorDestacada: Boolean read FCorDestacada write SetCorDestacada;
      property EntityPai: TEntity read FEntityPai;
      property EntityFilha: TEntity read FEntityFilha;
  end;

  TRelationshipArrowPositioner = class
    private
      FEntityContainer: TEntityContainer;
      FListArrow: TStringList;
      procedure analisarPossibilidadeDeSetas(relationship: TEntityRelationship);
      function descobrirSegmentosSemInterseccao(relationship1: TEntityRelationship): TSegmentosSemInterseccao;
      procedure criarSetas(shape: TShape; segmento: TPoint; posicaoSeta: TSentidoSeta; qtdeSetas, comprimento: Integer);
    public
      procedure PosicionarSetas;
      procedure DestruirSetas;
      constructor Create(entityContainer: TEntityContainer);
      destructor Destroy; override;
  end;

  TPopMenuEntity = class(TComponent)
    private
      FPopupMenu: TPopupMenu;
      FExibirTodosOsCampos: TMenuItem;
      FExibirTriggers: TMenuItem;
      FAmostraDeDados: TMenuItem;
      FSeparador1: TMenuItem;
      FSeparador2: TMenuItem;
      FRemoverDoDiagrama: TMenuItem;
      FPesquisaFilhos: TMenuItem;
      FPesquisaPais: TMenuItem;
      FEntity: TObject;
      procedure PesquisarTabelasFilhas(Sender: TObject);
      procedure PesquisarTabelasPai(Sender: TObject);
      procedure RemoverTabelaDoDiagrama(Sender: TObject);
      procedure ExibirTodosOsCampos(Sender: TObject);
      procedure ExibirTriggers(Sender: TObject);
      procedure AmostraDeDados(Sender: TObject);
    public
      constructor Create(AOwner: TComponent); reintroduce;
      procedure PopUp(Entity: TObject);
  end;

implementation

uses uObterMetaDados, uEntityRelationshipArrowShape, uVisualizarTriggers,
  uPesquisarRelacionamentos, uVariaveisGlobais, uPrincipal;

{ TEntityContainer }

procedure TEntityContainer.AddEntity(owner, tabela: string; top, left: Integer; exibirTodosOsCampos: Boolean);
var
  t: TEntity;
begin
  owner := UpperCase(owner);
  tabela := UpperCase(tabela);

  if FindEntity(owner, tabela) = nil then
  begin
    if TObterMetaDados.TabelaExiste(owner, tabela) then
    begin
      t := TEntity.Create(FEntityArea, tabela, owner, Self, exibirTodosOsCampos);
      // posicao inicial da forma
      t.Top := top;
      t.Left := left;
      FListEntity.AddObject(owner + '.' + tabela, t);
      LigarRelacionamentos;
    end
    else
      Application.MessageBox('Tabela não existe no banco de dados!',
        'ATENÇÃO', MB_OK + MB_ICONSTOP);

  end
  else
    Application.MessageBox('Está tabela já está presente no diagrama!',
      'ATENÇÃO', MB_OK + MB_ICONWARNING);

end;

constructor TEntityContainer.Create(OwnerParent: TWinControl);
begin
  // cria o scroll box
  FEntityArea := TScrollBox.Create(OwnerParent);
  //FEntityArea.CanFocus := True;
  FEntityArea.Parent := OwnerParent;
  FEntityArea.Align := alClient;
  FEntityArea.BorderStyle := bsNone;
  FEntityArea.Color := clWhite;
  FEntityArea.ParentColor := False;
  // cria o objeto de lista de entidades adicionadas
  FListEntity := TStringList.Create;
  FListRelacionamento := TStringList.Create;
  // cria o pop menu da entidade
  FPopMenuEntity := TPopMenuEntity.Create(OwnerParent);
  FRelationshipArrowPositioner := TRelationshipArrowPositioner.Create(Self);
end;

destructor TEntityContainer.Destroy;
var
  i: Integer;
begin
  TObject(FRelationshipArrowPositioner).Free;

  for i := FListRelacionamento.Count - 1 downto 0 do
    FListRelacionamento.Objects[i].Free;
  FListRelacionamento.Free;

  for i := FListEntity.Count - 1 downto 0 do
    FListEntity.Objects[i].Free;
  FListEntity.Free;

  FPopMenuEntity.Free;
  FEntityArea.Free;

  inherited Destroy;
end;

function TEntityContainer.FindEntity(Owner, TableName: string): TEntity;
var
  onde: Integer;
begin
  onde := FListEntity.IndexOf(Owner + '.' + TableName);
  if onde > -1 then
    Result := TEntity(FListEntity.Objects[onde])
  else
    Result := nil;
end;

function TEntityContainer.FindRelacionamento(ConstraintOwner,
  ConstraintName: string): TEntityRelationship;
var
  onde: Integer;
begin
  onde := FListRelacionamento.IndexOf(ConstraintOwner + '.' + ConstraintName);
  if onde > -1 then
    Result := TEntityRelationship(FListRelacionamento.Objects[onde])
  else
    Result := nil;
end;

procedure TEntityContainer.LigarRelacionamentos;
var
  i, k: Integer;
  entity, entityPai: TEntity;
  constraint: TEntityConstraint;
  relacionamento: TEntityRelationship;
  jaEstaRelacionado: TEntityRelationship;
  nomeColunaFilha: string;

  procedure BuscarAutoRelacionaentoSetaFilha;
  var
    m, onde, index : Integer;
    PKColumns: TStringList;
    FKColumns: TStringList;
  begin
    PKColumns := TStringList.Create;
    FKColumns := TStringList.Create;
    entity.GetColumnsNamesPK(PKColumns);
    entity.GetColumnsNamesFK(FKColumns, constraint.ConstraintOwner, constraint.ConstraintName);
    // escolhe a coluna que não está entre as PKs

    index := 0;
    for m := 0 to FKColumns.Count - 1 do
    begin
      onde := PKColumns.IndexOf(FKColumns[m]);
      if onde= -1 then
      begin
        index := m;
        Break;
      end;
    end;

    // bug DDL
    // neste ponto, conseguiram criar um auto-relacionamento
    // de uma coluna para ela mesma, então mudei o código
    // para o index menor ser zero index := 0;
    // ficando 'resolvido' a interface do diagrama 
    nomeColunaFilha := FKColumns[index];
    PKColumns.Free;
    FKColumns.Free;
  end;

begin
  // corre todas as entidades ligado seus relacionamentos
  for i := 0 to FListEntity.Count - 1 do
  begin
    entity := TEntity(FListEntity.Objects[i]);
    // corre todas as chaves estrangeiras
    for k := 0 to entity.ListConstraints.Count - 1 do
    begin
      constraint := TEntityConstraint(entity.ListConstraints.Objects[k]);

      // todas as FKs deverão ter setinha se a tabela pai estiver na tela
      // se essa setinha de constraint de FK nao tiver sido criada na tela, entao cria
      jaEstaRelacionado := FindRelacionamento(constraint.ConstraintOwner,constraint.ConstraintName);
      if jaEstaRelacionado = nil then
      begin
        // verifica se a tabela pai esta na tela
        entityPai := FindEntity(constraint.OwnerTabelaRelacionada, constraint.TabelaRelacionada);
        // se estiver no modelo, cria os relacionamentos
        if entityPai <> nil then
        begin
          // cria o relacionamento
          relacionamento := TEntityRelationship.Create(FEntityArea, constraint.ConstraintOwner, constraint.ConstraintName, Self);
          nomeColunaFilha := constraint.ConstraintColumnName;
          // se for um auto-relacionamento
          // encontra uma coluna que são faz parte da chave primária para criar a setinha filha
          if entity = entityPai then
            BuscarAutoRelacionaentoSetaFilha;

          relacionamento.SetarEntidadeFilha(entity, nomeColunaFilha);
          relacionamento.SetarEntidadePai(entityPai, constraint.ColunaRelacionada);
          RegistrarRelacionamento(constraint.ConstraintOwner, constraint.ConstraintName, relacionamento);
        end;
      end;
    end;
  end;
end;

procedure TEntityContainer.MoverTudo(direcao: TPoint);
var
  i: Integer;
begin
  for i := 0 to FListEntity.Count - 1 do
  begin
    TEntity(FListEntity.Objects[i]).Top := TEntity(FListEntity.Objects[i]).Top + direcao.Y;
    TEntity(FListEntity.Objects[i]).Left := TEntity(FListEntity.Objects[i]).Left + direcao.X;
    TEntity(FListEntity.Objects[i]).NotificarArrasto;
  end;
  FRelationshipArrowPositioner.PosicionarSetas;
end;

procedure TEntityContainer.NotificarDestaquePaisEFilhos(constraint: TEntityConstraint;
  Destacada: Boolean);
var
  i: Integer;
begin
  for i := 0 to FListEntity.Count - 1 do
  begin
    TEntity(FListEntity.Objects[i]).DestacarConstraint(constraint, Destacada);
  end;
end;

procedure TEntityContainer.NotificarFimArrasto;
begin
  if not FNaoRenderizarArrows then
    FRelationshipArrowPositioner.PosicionarSetas;
end;

procedure TEntityContainer.NotificarInicioArrasto;
begin
  if not FNaoRenderizarArrows then
    FRelationshipArrowPositioner.DestruirSetas;
end;

procedure TEntityContainer.RegistrarRelacionamento(ConstraintOwner,
  ConstraintName: string; Relacionamento: TEntityRelationship);
begin
  FListRelacionamento.AddObject(ConstraintOwner + '.' + ConstraintName, TEntityRelationship(Relacionamento));
end;

procedure TEntityContainer.RemoveEntity(entity: TEntity);
var
  i: Integer;
  relation: TEntityRelationship;
begin
  // remove todos os relacionamentos que essa tabela tem
  for i := FListRelacionamento.Count - 1 downto 0 do
  begin
    relation := TEntityRelationship(FListRelacionamento.Objects[i]);
    if (relation.EntityPai = entity) or (relation.EntityFilha = entity) then
    begin
      relation.EntityPai.DesregistrarRelacionamento(relation);
      relation.EntityFilha.DesregistrarRelacionamento(relation);
      TObject(relation).Free;
      FListRelacionamento.Delete(i);
    end;
  end;
  // remove a tabela
  TObject(entity).Free;
  FListEntity.Delete(FListEntity.IndexOfObject(TObject(entity)));
end;

procedure TEntityContainer.ScreenShot;
{var
  DC : HDC;
  DestBitmap: TBitmap;}
begin
  {DC := GetDC (FEntityArea.Handle);
  try
    DestBitmap := TBitmap.Create;
    DestBitmap.Width := GetDeviceCaps (DC, HORZRES) ;
    DestBitmap.Height := GetDeviceCaps (DC, VERTRES) ;
    BitBlt(DestBitmap.Canvas.Handle, 0, 0, DestBitmap.Width, DestBitmap.Height, DC, 0, 0, SRCCOPY);
    DestBitmap.SaveToFile('c:\export.bmp');
    Application.MessageBox('Exportado para c:\export.bmp', 'ATENÇÃO', MB_OK +
      MB_ICONINFORMATION);

  finally
    ReleaseDC (GetDesktopWindow, DC) ;
  end;}
end;

procedure TEntityContainer.SetNaoRenderizarArrows(Value: Boolean);
begin
  FNaoRenderizarArrows := Value;
  if not Value then
    FRelationshipArrowPositioner.PosicionarSetas;
end;

{ TEntity }

procedure TEntity.AddColunaNaoChave(CaptionLinha: string; TipoLinha: TTipoLinha;
  Hint: string);
var
  x: TEntityColumn;
begin
  x := TEntityColumn.Create(Self, CaptionLinha, TipoLinhaNaoChave, Hint);

  // define a posição vertical e horizontal do linha
  if (FListColunasChave.Count + FListColunasNaoChave.Count) > 0 then
  begin
    x.Top := (csLineHeight * (FListColunasChave.Count + FListColunasNaoChave.Count)) + 1;
  end
  else
  begin
    x.Top := 1;
  end;
  x.Left := csLineMargin;
  FListColunasNaoChave.AddObject(CaptionLinha, x);
  NotificarMudancaDeHeigth;
end;

function TEntity.AddColunaConstraint(CaptionLinha: string;
  TipoLinha: TTipoLinha; Hint, ConstraintOwner, ConstraintName, ConstraintColumnName: string; PosicaoTop: Integer; ROwner, RConstraintName: string): TEntityConstraint;
var
  x: TEntityConstraint;
  //i: Integer;
begin
  // verifica se ja tem uma representação FK criada para a coluna em questão
  //for i := 0 to FListConstraints.Count - 1 do
  //  if TEntityConstraint(FListConstraints.Objects[i]).ConstraintColumnName = ConstraintColumnName then
  //    Break;

  // se ja tiver, entao não criar de novo
  //if i <= FListConstraints.Count -1 then

  x := TEntityConstraint.Create(Self, CaptionLinha, TipoLinha, Hint);
  x.ConstraintOwner := ConstraintOwner;
  x.ConstraintName := ConstraintName;
  x.ConstraintColumnName := ConstraintColumnName;
  x.ROwner := ROwner;
  x.RConstraintName := RConstraintName;



  // deterna a posição do shape
  x.Top := PosicaoTop;
  x.Height := csLineHeight;
  x.Width := csConstraintWidth;
  if TipoLinha = TipoLinhaFK then
    x.Left := 1
  else
    x.Left := 12;

  // na a contraint na lista de constraints adicionadas
  FListConstraints.AddObject(ConstraintName, x);
  Result := x;
end;

function TEntity.AddColunaChave(CaptionLinha: string; TipoLinha: TTipoLinha; Hint: string): TCustomEntityColumn;
var
  x: TEntityColumn;
  jaTem: Integer;
begin
  jaTem := FListColunasChave.IndexOf(CaptionLinha);
  // só adiciona a linha se ela não existir na entidade
  if (jaTem = -1) then
  begin
    x := TEntityColumn.Create(Self, CaptionLinha, TipoLinha, Hint);

    // define a posição vertical e horizontal do linha
    if FListColunasChave.Count > 0 then
    begin
      x.Top := (csLineHeight * FListColunasChave.Count) + 1;
    end
    else
    begin
      x.Top := 1;
    end;
    x.Left := csLineMargin;
    FListColunasChave.AddObject(CaptionLinha, x);
    NotificarMudancaDeHeigth;
    Result := x;
  end
  else
    Result := TCustomEntityColumn(FListColunasChave.Objects[jaTem]);
end;

constructor TEntity.Create(AOwner: TComponent; NomeTabela, Owner: string; EntityContainer: TEntityContainer; ExibirTodosOsCampos: Boolean);
begin
  inherited Create(AOwner);
  // seta a dimensão inicial
  Width := LarguraInicialEntity;
  Height := 5;
  // inicia a contrucao da entidade
  Screen.Cursor := crHourGlass;
  Visible := False;
  // seta o entity container da entidade
  FEntityContainer := EntityContainer;
  // aplica a forma da entidade
  Parent := TWinControl(AOwner);
  FShapeBorda := TShape.Create(Self);
  FShapeBorda.Brush.Color := clWhite;
  FShapeBorda.Align := alClient;
  FShapeBorda.Pen.Color := clGray;
  FShapeBorda.Parent := Self;
  BevelOuter  := bvNone;

  // cria a lista de relacionamentos
  FListRelacionamentos := TStringList.Create;

  // cria as linhas da entidade
  FListColunasChave := TStringList.Create;
  FListColunasNaoChave := TStringList.Create;
  FExibindoTodosOsCampos := False;
  FListConstraints := TStringList.Create;
  // adiciona o nome das entidades lista de linhas
  AddColunaChave(LowerCase(NomeTabela), TipoLinhaNomeTabela, 'Owner: ' + Owner);
  // seta as propriedades
  FNomeTabela := UpperCase(NomeTabela);
  FSchemaOnwer := UpperCase(Owner);
  //Insere a lista de campos na linhas da entidade
  TObterMetaDados.ObterChavesDaEntidade(Self);
  // incluiu ou não campos não chave
  FExibindoTodosOsCampos := ExibirTodosOsCampos;
  if ExibirTodosOsCampos then
    TObterMetaDados.ObterCamposNaoChave(Self);

  // finaliza a contrução da entidade
  Visible := True;
  Screen.Cursor := crDefault;
end;

procedure TEntity.DesregistrarRelacionamento(Relacionamento: TEntityRelationship);
begin
  FListRelacionamentos.Delete(FListRelacionamentos.IndexOfObject(TEntityRelationship(Relacionamento)));
end;

destructor TEntity.Destroy;
var
  i: Integer;
begin
  // libera as colunas da tabela
  for i := 0 to FListColunasChave.Count - 1 do
     TObject(FListColunasChave.Objects[i]).Free;
  FListColunasChave.Free;

  // libera as colunas da tabela
  RemoverColunasNaoChave(True);
  FListColunasNaoChave.Free;

  //  libera as constraints da tabela
  for i := 0 to FListConstraints.Count - 1 do
     TObject(FListConstraints.Objects[i]).Free;
  FListConstraints.Free;

  // apenas libera a lista pois o objetos são liberados no EntityContainer, onde eles são criados
  FListRelacionamentos.Free;
  //
  FShapeBorda.Free;
  inherited;
end;

procedure TEntity.GetColumnsNamesFK(buffer: TStringList; Owner,
  ConstraintName: string);
var
  i: Integer;
  constraint: TEntityConstraint;
begin
  for i := 0 to FListConstraints.Count - 1 do
  begin
    constraint := TEntityConstraint(FListConstraints.Objects[i]);
    if (constraint.TipoLinha = TipoLinhaFK) and (constraint.ConstraintOwner = Owner) and (constraint.ConstraintName = ConstraintName) then
      buffer.Add(constraint.ConstraintColumnName);
  end;
end;

procedure TEntity.GetColumnsNamesPK(buffer: TStringList);
var
  i: Integer;
  constraint: TEntityConstraint;
begin
  for i := 0 to FListConstraints.Count - 1 do
  begin
    constraint := TEntityConstraint(FListConstraints.Objects[i]);
    if constraint.TipoLinha = TipoLinhaPK then
      buffer.Add(constraint.ConstraintColumnName);
  end;
end;

function TEntity.GetEntityColumnByName(NomeColuna: string): TCustomEntityColumn;
begin
  Result := TCustomEntityColumn(FListColunasChave.Objects[FListColunasChave.IndexOf(NomeColuna)]);
end;

// evendo recebido da linha, que na verdade arrasta a entidade
// deve notificar os relacionamentos da mudança de posição das entidades
procedure TEntity.NotificarArrasto;
var
  i: Integer;
begin
  for i := 0 to FListRelacionamentos.Count - 1 do
    TEntityRelationship(FListRelacionamentos.Objects[i]).NotificarArrasto;
end;

procedure TEntity.DestacarConstraint(constraint: TEntityConstraint; Destacada: Boolean);

  procedure destacarRelacionamento(cor: TColor; ownerDotConstraintName: string);
  var
    index: Integer;
  begin
    // destaca o relacionamento
    index := FListRelacionamentos.IndexOf(ownerDotConstraintName);
    if index > -1 then
      if Destacada then
        TEntityRelationship(FListRelacionamentos.Objects[index]).AplicarNovaCor(cor)
      else
        TEntityRelationship(FListRelacionamentos.Objects[index]).RemoverCor(cor);
  end;

var
  i: Integer;
  auxConstraint: TEntityConstraint;
begin
  for i := 0 to FListConstraints.Count - 1 do
  begin
    // destaca a chaves primária
    auxConstraint := TEntityConstraint(FListConstraints.Objects[i]);
    if (auxConstraint.ConstraintOwner = constraint.ConstraintOwner) and (auxConstraint.ConstraintName = constraint.ConstraintName) then
    begin
      auxConstraint.CorDestacada := Destacada;
      GetEntityColumnByName(auxConstraint.ConstraintColumnName).CorDestacada := Destacada;
    end;


    if constraint.TipoLinha = TipoLinhaPK then
    begin
      // destaca as chaves estrangeiras
      if (auxConstraint.ROwner = constraint.ConstraintOwner) and (auxConstraint.RConstraintName = constraint.ConstraintName) then
      begin
        auxConstraint.CorDestacada := Destacada;
        with GetEntityColumnByName(auxConstraint.ConstraintColumnName) do
        begin
          CorDestacada := Destacada;
          BringToFront;
        end;

        destacarRelacionamento(clRed, auxConstraint.ConstraintOwner + '.' + auxConstraint.ConstraintName);
      end;
    end
    else
    begin
      // destaca as chaves primárias
      if (auxConstraint.ConstraintOwner = constraint.ROwner) and (auxConstraint.ConstraintName = constraint.RConstraintName) then
      begin
        auxConstraint.CorDestacada := Destacada;
        with GetEntityColumnByName(auxConstraint.ConstraintColumnName) do
        begin
          CorDestacada := Destacada;
           BringToFront;
        end;

        destacarRelacionamento(clBlue, constraint.ConstraintOwner + '.' + constraint.ConstraintName);
      end;
    end;
  end;
end;

procedure TEntity.NotificarMudancaDeHeigth;
begin
  Height := (FListColunasChave.Count + FListColunasNaoChave.Count) * csLineHeight + 2;
end;

{procedure TEntity.NotificarEsmaecerCorConstraint(ConstratintName: string);
var
  i: Integer;
begin
  for i := 0 to FListConstraints.Count - 1 do
    if TEntityConstraint(FListConstraints.Objects[i]).ConstraintName = ConstratintName then
      TEntityConstraint(FListConstraints.Objects[i]).EsmaecerCor;
end;

procedure TEntity.NotificarIntensificarCorConstraint(
  ConstratintName: string);
var
  i: Integer;
begin
  for i := 0 to FListConstraints.Count - 1 do
    if TEntityConstraint(FListConstraints.Objects[i]).ConstraintName = ConstratintName then
    begin
      TEntityConstraint(FListConstraints.Objects[i]).BringToFront;
      TEntityConstraint(FListConstraints.Objects[i]).IntensificarCor;
    end;
end;}

procedure TEntity.NotificarNovaDimensao(NovaMaxWidthLinha: Word);
var
  i: Integer;
begin
  FMaxWidthLinha := NovaMaxWidthLinha;
  Width := FMaxWidthLinha + csLineMargin;

  for i := 0 to FListColunasChave.Count - 1 do
  begin
    if TEntityColumn(FListColunasChave.Objects[i]).Width < FMaxWidthLinha then
      TEntityColumn(FListColunasChave.Objects[i]).Width := FMaxWidthLinha;
  end;

  for i := 0 to FListColunasNaoChave.Count - 1 do
  begin
    if TEntityColumn(FListColunasNaoChave.Objects[i]).Width < FMaxWidthLinha then
      TEntityColumn(FListColunasNaoChave.Objects[i]).Width := FMaxWidthLinha;
  end;

  NotificarArrasto;
end;

procedure TEntity.RegistrarRelacionamento(Relacionamento: TEntityRelationship; SchemaOwner, ConstraintNameFK: string);
begin
  FListRelacionamentos.AddObject(SchemaOwner + '.' + ConstraintNameFK, TEntityRelationship(Relacionamento));
end;

procedure TEntity.RemoverColunasNaoChave(EstaFechandoDiagrama: boolean);
var
  i: Integer;
begin
  for i := 0 to FListColunasNaoChave.Count - 1 do
    TCustomEntityColumn(FListColunasNaoChave.Objects[i]).Free;

  FListColunasNaoChave.Clear;
  NotificarMudancaDeHeigth;
  ExibindoTodosOsCampos := False;

  if not EstaFechandoDiagrama then
  begin
    FMaxWidthLinha := 0;
    Width := LarguraInicialEntity;
    for i := 0 to ListColunasChave .Count - 1 do
      TCustomEntityColumn(FListColunasChave.Objects[i]).DeterminarDimensoes;
  end;
end;

{ TCustomEntityColumn }

constructor TCustomEntityColumn.Create(AOwner: TComponent; Caption: string; TipoLinha: TTipoLinha; Hint: string);
begin
  inherited Create(AOwner);
  Self.FCountIntensificarCor := 1;
  Self.Caption := Caption;
  Self.TipoLinha := TipoLinha;
  Parent := TWinControl(AOwner);
  Self.Hint := Hint;
  Height := 0;
  ShowHint := True;
  Canvas.Pen.Color := clBlack;
  Pen.Style := psClear;
  DeterminarDimensoes;
  FCorDestacada := False;
end;

procedure TCustomEntityColumn.DeterminarDimensoes;
var
  t: tagSize;
  largura: Word;
begin
  // auto redimensiona o shape apenas se for esses tipo de linha abaixo,
  // senão usa largura fixa para PK e FK
  if FTipoLinha in [TipoLinhaNomeTabela, TipoLinhaNomeColuna, TipoLinhaNaoChave] then
  begin
    // reseta a width
    Width := Parent.Width - (2 * csConstraintWidth);
    // calcula a largura necessária para o shape
    t := Canvas.TextExtent(FCaption);
    largura := t.cx + 6;

    if largura > Width then
    begin
      Width := largura;
      TEntity(Parent).NotificarNovaDimensao(largura);
    end
    else
    begin
      Width := TEntity(Parent).MaxWidthLinha;
    end;

    if Height <> csLineHeight then
    begin
      Height := csLineHeight;
    end;
  end;
end;

procedure TCustomEntityColumn.EsmaecerCor;
begin
  // decrementa a pilha de pedidos de intensificar cor
  if FCountIntensificarCor > 0 then
    Dec(FCountIntensificarCor);
    
  //MemoLog.Lines.Add('chamou esmaecer, decrementou para: ' + IntToStr(FCountIntensificarCor));

  if FCountIntensificarCor = 0 then
  begin
    case FTipoLinha of
      TipoLinhaNomeTabela:
      begin
        Brush.Color := $00FFC4E1; // roxinho
        Canvas.Font.Style := [fsBold];
      end;
      TipoLinhaNomeColuna:
      begin
        if not FCorDestacada then
          Brush.Color := $00E7FADE; // verdinho
      end;
      TipoLinhaPK:
      begin
        if not FCorDestacada then
          Brush.Color := $00E0E1FE; // vermelhinho
      end;
      TipoLinhaFK:
      begin
        if not FCorDestacada then
          Brush.Color := $00FFEFDF; // azulzinho
      end;
      TipoLinhaNaoChave:
      begin
        Brush.Color := $00F3F1F3; // cinzinha
      end;
    end;
  end;
end;

procedure TCustomEntityColumn.IntensificarCor;
begin
  // incrementa a pilha de pedidos de intensificar cor
  Inc(FCountIntensificarCor);
  //MemoLog.Lines.Add('chamou intensifica, incrementou para: ' + IntToStr(FCountIntensificarCor));

  case FTipoLinha of
    TipoLinhaNomeTabela: Brush.Color := $00FF91C8; // roxo
    TipoLinhaNomeColuna: Brush.Color := $00C9F3B4; // verde
    TipoLinhaPK: Brush.Color := $009B9DFB; // vermelho
    TipoLinhaFK: Brush.Color := $00FFD7AE;// azul
    TipoLinhaNaoChave: Brush.Color := $00E7E4E7; // cinza
  end;
  Self.BringToFront;
end;

procedure TCustomEntityColumn.Paint;
begin
  inherited Paint;
  Canvas.TextOut(3, 2, FCaption);
end;

procedure TCustomEntityColumn.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TCustomEntityColumn.SetCorDestacada(Value: Boolean);
begin
  FCorDestacada := Value;
  if Value then
    IntensificarCor
  else
    EsmaecerCor;
end;

procedure TCustomEntityColumn.SetTipoLinha(Value: TTipoLinha);
begin
  FTipoLinha := Value;
  case FTipoLinha of
    TipoLinhaNomeTabela:
    begin
      OnMouseMove := ShapeMouseMove;
      OnMouseUp := ShapeMouseUp;
      OnMouseDown := ShapeMouseDown;
    end;
    TipoLinhaNomeColuna:
    begin
      OnMouseMove := nil;
      OnMouseUp := nil;
      OnMouseDown := nil;
    end;
    TipoLinhaPK:
    begin
      OnMouseMove := nil;
      OnMouseUp := nil;
      OnMouseDown := nil;
      OnClick := ChaveSingleClick;
    end;
    TipoLinhaFK:
    begin
      OnMouseMove := nil;
      OnMouseUp := nil;
      OnMouseDown := nil;
      OnDblClick := FKDoubleClick;
      OnClick := ChaveSingleClick;
    end;
    TipoLinhaNaoChave:
    begin
      OnMouseMove := nil;
      OnMouseUp := nil;
      OnMouseDown := nil;
    end;
  end;
  OnMouseEnter := ShapeMouseEnter;
  OnMouseLeave := ShapeMouseLeave;
  EsmaecerCor;
end;

procedure TCustomEntityColumn.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetCapture(Parent.Handle);
    FCapturing := true;
    FMouseDownSpot.X := x;
    FMouseDownSpot.Y := Y;
    TEntityContainer(TEntity(Parent).EntityContainer).NotificarInicioArrasto;
  end
  else
  begin
    TEntityContainer(TEntity(Parent).EntityContainer).PopMenuEntity.PopUp(Owner);
  end;
end;

procedure TCustomEntityColumn.ShapeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FCapturing then
  begin
    Parent.Left := Parent.Left - (FMouseDownSpot.x - x);
    Parent.Top := Parent.Top - (FMouseDownSpot.y - y);
    TEntity(Parent).NotificarArrasto;
    {if Parent.Left < 0 then
      Parent.Left := 0;
    if Parent.Top < 0 then
      Parent.Top := 0;}
  end;
end;

procedure TCustomEntityColumn.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCapturing then
  begin
    ReleaseCapture;
    FCapturing := False;
    Parent.Left := Parent.Left - (FMouseDownSpot.x -x);
    Parent.Top := Parent.Top - (FMouseDownSpot.y - y);
    TEntity(Parent).NotificarArrasto;
    TEntityContainer(TEntity(Parent).EntityContainer).NotificarFimArrasto;
  end;
end;

procedure TCustomEntityColumn.ShapeMouseEnter(Sender: TObject);
begin
  IntensificarCor;
end;

procedure TCustomEntityColumn.ShapeMouseLeave(Sender: TObject);
begin
  EsmaecerCor;
end;

{ TEntityConstraint }

procedure TEntityConstraint.FKDoubleClick(Sender: TObject);
begin
  FDestacadaPorClick := False; // trambique para evitar o relacionamento iniciar azul
  TEntity(Self.Owner).EntityContainer.NotificarDestaquePaisEFilhos(Self, False); // trambique para evitar o relacionamento iniciar azul
  TEntityContainer(TEntity(Self.Owner).EntityContainer).AddEntity(FOwnerTabelaRelacionada, FTabelaRelacionada, 3, 3, False);
  //MemoLog.Lines.Add('double click');
end;

procedure TEntityConstraint.ChaveSingleClick(Sender: TObject);
begin
  FDestacadaPorClick := not FDestacadaPorClick;
  TEntity(Self.Owner).EntityContainer.NotificarDestaquePaisEFilhos(Self, FDestacadaPorClick);
  //MemoLog.Lines.Add('single click');
end;

procedure TEntityConstraint.ShapeMouseEnter(Sender: TObject);
begin
  // não precisa executar o código herdado pois no metodo abaixo vai mudar a cor de qq forma

  if not FDestacadaPorClick then
    TEntity(Self.Owner).EntityContainer.NotificarDestaquePaisEFilhos(Self, True);
end;

procedure TEntityConstraint.ShapeMouseLeave(Sender: TObject);
begin
  // não precisa executar o código herdado pois no metodo abaixo vai mudar a cor de qq forma

  if not FDestacadaPorClick then
    TEntity(Self.Owner).EntityContainer.NotificarDestaquePaisEFilhos(Self, False);
end;

{ TEntityRelationship }

procedure TEntityRelationship.AplicarNovaCor(cor: TColor);
begin
  FHistoricoDeCor.Add(IntToStr(cor));

  FVT.BringToFront;
  FVT.Pen.Color := cor;
  FHR1.BringToFront;
  FHR1.Pen.Color := cor;
  FHR2.BringToFront;
  FHR2.Pen.Color := cor;
end;

procedure TEntityRelationship.AplicarPosicaoCalculada;
begin
  FVT.Top := FPosiVT.Top;
  FVT.Left := FPosiVT.Left;
  FVT.Height := FPosiVT.Height;

  FHR1.Top := FPosiHR1.Top;
  FHR1.Left := FPosiHR1.Left;
  FHR1.Width := FPosiHR1.Width;

  FHR2.Top := FPosiHR2.Top;
  FHR2.Left := FPosiHR2.Left;
  FHR2.Width := FPosiHR2.Width;
end;

procedure TEntityRelationship.Calcular(pAplicarPosicaoCalculada: Boolean);
{
como lados opostos nunca se encontram com 3 retas que não fazem interseção com os planos dos retangulos,
nunca calcular a possibilidade A-D

 -----
A|   |B
 |   |
 -----

            ------
           C|    |D
            |    |
            ------
}

var
  a, b, c, d: TPoint;
  ac, bc, bd: Integer;
  listOrdenada: TStringList;
begin
  // calcula os pontos segundo a posição das entidades pai e filha
  FPontoA.X := TPanel(FEntityFilha).Left;
  FPontoA.Y := TPanel(FEntityFilha).Top + TPanel(FEntityFilha.GetEntityColumnByName(FLinhaNomeColunaFilha)).Top + 9;
  FPontoB.X := TPanel(FEntityFilha).Left + TPanel(FEntityFilha).Width;
  FPontoB.Y := FPontoA.Y;

  FPontoC.X := TPanel(FEntityPai).Left;
  FPontoC.Y := TPanel(FEntityPai).Top + TPanel(FEntityPai.GetEntityColumnByName(FLinhaNomeColunaPai)).Top + 9;
  FPontoD.X := TPanel(FEntityPai).Left + TPanel(FEntityPai).Width;
  FPontoD.Y := FPontoC.Y;

  // colocar a forma mais a esquerda nos pontos A e B
  if FPontoA.X <= FPontoC.X then
  begin
    a := FPontoA;
    b := FPontoB;
    c := FPontoC;
    d := FPontoD;
  end
  else // o inverso
  begin
    a := FPontoC;
    b := FPontoD;
    c := FPontoA;
    d := FPontoB;
  end;

  // calcular as distancias a-c, b-c e b-d
  // e usar o menor caminho para fazer a setinha
  // lado externo com interno
  ac := Abs(a.X - c.X) + Abs(a.Y - c.Y) + (2 * FDistanciaLateral);
  // lados internos
  bc := Abs(b.X - c.X) + Abs(b.Y - c.Y);
  // lado interno com externo
  bd := Abs(b.X - d.X) + Abs(b.Y - d.Y) + (2 * FDistanciaLateral);

  listOrdenada := TStringList.Create;
  listOrdenada.Add(Format('%8.0d', [ac]) + 'ac');
  // este caminho só pode ser usado se passar por este teste
  if (c.X - b.X) > (DistanciaLateralMinima * 2) then
    listOrdenada.Add(Format('%8.0d', [bc]) + 'bc');
  listOrdenada.Add(Format('%8.0d', [bd]) + 'bd');
  listOrdenada.Sorted := True;

  FNomeCaminhoUsado := Copy(listOrdenada[0],9,2);

  if FNomeCaminhoUsado = 'bc' then
  begin
    FPosiHR1.Top := b.Y;
    FPosiHR1.Left := b.X;
    FPosiHR1.Width := FDistanciaLateral;

    // impede que o height fique negativo
    if b.Y < c.Y then
    begin
      FPosiVT.Top := b.Y;
      FPosiVT.Height := c.Y - b.Y;
    end
    else
    begin
      FPosiVT.Top := c.Y;
      FPosiVT.Height := b.Y - c.Y;
    end;
    FPosiVT.Left := b.X + FDistanciaLateral;

    FPosiHR2.Top := c.Y;
    FPosiHR2.Left := FPosiVT.Left;
    FPosiHR2.Width := c.X - b.X - FDistanciaLateral;

    if FPosiHR1.Left < FPontoA.x then
      FSentidoHR1 := psEsquerda
    else
      FSentidoHR1 := psDireita;

    if FPosiHR2.Left < FPontoA.x then
      FSentidoHR2 := psEsquerda
    else
      FSentidoHR2 := psDireita;

    if FPosiVT.Top < FPontoA.Y then
      FSentidoVT := psCima
    else
      FSentidoVT := psBaixo;
  end
  else if FNomeCaminhoUsado = 'ac' then
  begin
    FPosiHR1.Top := a.Y;
    FPosiHR1.Left := a.X - FDistanciaLateral;
    FPosiHR1.Width := FDistanciaLateral;

    // impede que o height fique negativo
    if a.Y < c.Y then
    begin
      FPosiVT.Top := a.Y;
      FPosiVT.Height := c.Y - a.Y;
    end
    else
    begin
      FPosiVT.Top := c.Y;
      FPosiVT.Height := a.Y - c.Y;
    end;
    FPosiVT.Left := FPosiHR1.Left;

    FPosiHR2.Top := c.Y;
    FPosiHR2.Left := FPosiVT.Left;
    FPosiHR2.Width := c.X - a.X + FDistanciaLateral;

    if FPosiHR1.Top = FPontoA.Y then
    begin
      FSentidoHR1 := psEsquerda;
      FSentidoHR2 := psDireita;
    end
    else
    begin
      FSentidoHR1 := psDireita;
      FSentidoHR2 := psEsquerda;
    end;

    if FPosiVT.Top < FPontoA.Y then
      FSentidoVT := psCima
    else
      FSentidoVT := psBaixo;
  end
  else if FNomeCaminhoUsado = 'bd' then
  begin
    FPosiHR1.Top := b.Y;
    FPosiHR1.Left := b.X;
    FPosiHR1.Width := FDistanciaLateral + IfThen(d.X - b.X > 0, d.X - b.X, 0);

    // impede que o height fique negativo
    if b.Y < d.Y then
    begin
      FPosiVT.Top := b.Y;
      FPosiVT.Height := d.Y - b.Y;
    end
    else
    begin
      FPosiVT.Top := d.Y;
      FPosiVT.Height := b.Y - d.Y;
    end;
    FPosiVT.Left := FPosiHR1.Left + FPosiHR1.Width;

    FPosiHR2.Top := d.Y;
    FPosiHR2.Left := d.X;
    FPosiHR2.Width := FPosiVT.Left - d.X;

    if FPosiHR1.Top = FPontoA.Y then
    begin
      FSentidoHR1 := psDireita;
      FSentidoHR2 := psEsquerda;
    end
    else
    begin
      FSentidoHR1 := psEsquerda;
      FSentidoHR2 := psDireita;
    end;

    if FPosiVT.Top < FPontoA.Y then
      FSentidoVT := psCima
    else
      FSentidoVT := psBaixo;

  end;

  if pAplicarPosicaoCalculada then
  begin
    AplicarPosicaoCalculada;

    {if FConstraintname = 'LOCALIDADE#PACIENTE_FK' then
    begin
      MemoLog.Lines.Add('a.X: ' + IntToStr(a.X) + ' a.y: ' + IntToStr(a.Y));
      MemoLog.Lines.Add('b.X: ' + IntToStr(b.X) + ' b.y: ' + IntToStr(b.Y));
      MemoLog.Lines.Add('c.X: ' + IntToStr(c.X) + ' c.y: ' + IntToStr(c.Y));
      MemoLog.Lines.Add('d.X: ' + IntToStr(d.X) + ' d.y: ' + IntToStr(d.Y));
      MemoLog.Lines.Add('distancia b.x - a.x: ' + IntToStr(b.X - a.X));
      MemoLog.Lines.Add('distancia d.x - c.x: ' + IntToStr(d.X - c.X));
      MemoLog.Lines.Add('Distancia Lateral: ' + IntToStr(DistanciaLateral));
      MemoLog.Lines.AddStrings(listOrdenada);
    end;}
  end;

  listOrdenada.Free;
end;

procedure TEntityRelationship.NotificarArrasto;
begin
  if (FEntityPai <> nil) and (FEntityFilha <> nil) then
    Calcular(True);
end;

constructor TEntityRelationship.Create(Owner: TWinControl; SchemaOwner, ConstraintName: string; EntityContainer: TEntityContainer);
begin
  // cria as tres linhas do relacionamento (setinha)
  // vt
  FVT := TEntityRelationshipDragShape.Create(Owner, Self);
  FVT.Parent := Owner;
  FVT.Width := 1;
  FVT.OnMouseEnter := ShapeMouseEnter;
  FVT.OnMouseLeave := ShapeMouseLeave;
  FVT.ShowHint := True;
  FVT.Hint := SchemaOwner + '.' + ConstraintName;
  // hr1
  FHR1 := TShape.Create(Owner);
  FHR1.Parent := Owner;
  FHR1.Height := 1;
  FHR1.OnMouseEnter := ShapeMouseEnter;
  FHR1.OnMouseLeave := ShapeMouseLeave;
  FHR1.ShowHint := True;
  FHR1.Hint := SchemaOwner + '.' + ConstraintName;
  // hr2
  FHR2 := TShape.Create(Owner);
  FHR2.Parent := Owner;
  FHR2.Height := 1;
  FHR2.OnMouseEnter := ShapeMouseEnter;
  FHR2.OnMouseLeave := ShapeMouseLeave;
  FHR2.ShowHint := True;
  FHR2.Hint := SchemaOwner + '.' + ConstraintName;
  //
  FDistanciaLateral := 10;
  FSchemaOwner := SchemaOwner;
  FConstraintname := ConstraintName;
  FEntityContainer := EntityContainer;
  FHistoricoDeCor := TStringList.Create;
  AplicarNovaCor(clGray); // inicialia a primeira cor
end;

destructor TEntityRelationship.Destroy;
begin
  FVT.Free;
  FHR1.Free;
  FHR2.Free;
  FHistoricoDeCor.Free;
  inherited Destroy;
end;

procedure TEntityRelationship.EsmaecerCor;
begin
  FVT.Pen.Color := clGray;
  FHR1.Pen.Color := clGray;
  FHR2.Pen.Color := clGray;
end;

procedure TEntityRelationship.IntensificarCor;
begin
  FVT.BringToFront;
  FVT.Pen.Color := clRed;
  FHR1.BringToFront;
  FHR1.Pen.Color := clRed;
  FHR2.BringToFront;
  FHR2.Pen.Color := clRed;
end;

procedure TEntityRelationship.SetarEntidadeFilha(entity: TEntity;
  NomeColuna: string);
begin
  FEntityFilha := entity;
  FLinhaNomeColunaFilha := NomeColuna;
  entity.RegistrarRelacionamento(Self, FSchemaOwner, FConstraintName);
  if (FEntityPai <> nil) and (FEntityFilha <> nil) then
    Calcular(True);
end;

procedure TEntityRelationship.SetarEntidadePai(entity: TEntity;
  NomeColuna: string);
begin
  FEntityPai := entity;

  FLinhaNomeColunaPai := NomeColuna;
  entity.RegistrarRelacionamento(Self, FSchemaOwner, FConstraintName);
  if (FEntityPai <> nil) and (FEntityFilha <> nil) then
    Calcular(True);
end;

procedure TEntityRelationship.SetCorDestacada(Value: Boolean);
begin
  FCorDestacada := Value;
  if Value then
    IntensificarCor
  else
    EsmaecerCor;
end;

procedure TEntityRelationship.SetDistanciaLateral(Value: Integer);
begin
  FDistanciaLateral := Value;
  // desenhar setinha apenas quando os pontos ABCD estiverem todos informados
  if (FEntityPai <> nil) and (FEntityFilha <> nil) then
    Calcular(False); // o código de arrastar determina aplicar na tela o arrasto
end;

procedure TEntityRelationship.SetNomeCaminhoUsado(Value: string);
begin
  if Value <> FNomeCaminhoUsado then
  begin
    FNomeCaminhoUsado := Value;
    // ** não mudar essa margem definida pelo usuário ao arrastar
    //DistanciaLateral := 10;
    // **
  end;
end;

procedure TEntityRelationship.ShapeMouseEnter(Sender: TObject);
begin
  //IntensificarCor;
  AplicarNovaCor(clBlack);
end;

procedure TEntityRelationship.ShapeMouseLeave(Sender: TObject);
begin
  //EsmaecerCor;
  RemoverCor(clBlack);
end;

procedure TEntityRelationship.RemoverCor(cor: TColor);
var
  onde: Integer;
begin
  onde := FHistoricoDeCor.IndexOf(IntToStr(cor));
  if onde > -1 then
    FHistoricoDeCor.Delete(onde);

  if FHistoricoDeCor.Count > 0 then
    cor := StrToInt(FHistoricoDeCor[FHistoricoDeCor.Count -1])
  else
    cor := clGray;

  FVT.BringToFront;
  FVT.Pen.Color := cor;
  FHR1.BringToFront;
  FHR1.Pen.Color := cor;
  FHR2.BringToFront;
  FHR2.Pen.Color := cor;
end;

{ TRelationshipArrowPositioner }

procedure TRelationshipArrowPositioner.analisarPossibilidadeDeSetas(relationship: TEntityRelationship);

  procedure chamarCriarSetas(segmentos: TArrayOfTPoint; shape: TShape; sentidoSeta: TSentidoSeta);
  var
    comprimento, i: Integer;
  begin
    for i := Low(segmentos) to High(segmentos) do
    begin
      comprimento := segmentos[i].Y - segmentos[i].X;
      // menor que 10px nao cabe seta alguma
      if comprimento > 10 then
      begin
        criarSetas(shape, segmentos[i], sentidoSeta, 1, comprimento);
      end;
    end;
  end;

var
  vetorInclusao: TSegmentosSemInterseccao;
begin
  // descobre onde pode ser setas
  vetorInclusao := descobrirSegmentosSemInterseccao(relationship);
  // chama criar setar para cada segmento da seta
  chamarCriarSetas(vetorInclusao.arrayVT, relationship.ShapeVT, relationship.SentidoVT);
  chamarCriarSetas(vetorInclusao.arrayHR1, relationship.ShapeHR1, relationship.SentidoHR1);
  chamarCriarSetas(vetorInclusao.arrayHR2, relationship.ShapeHR2, relationship.SentidoHR2);
end;

constructor TRelationshipArrowPositioner.Create(entityContainer: TEntityContainer);
begin
  FEntityContainer := entityContainer;
  FListArrow := TStringList.Create;
end;

procedure TRelationshipArrowPositioner.criarSetas(shape: TShape;
  segmento: TPoint; posicaoSeta: TSentidoSeta; qtdeSetas, comprimento: Integer);
var
  onde, i : Integer;
  seta: TEntityRelationshipArrowShape;
begin
  // distribui as setas uniformemente
  for i := 1 to qtdeSetas do
  begin
    onde := Trunc(comprimento / (qtdeSetas + 1));
    // cria a seta
    seta := TEntityRelationshipArrowShape.Create(FEntityContainer.EntityArea);
    FListArrow.AddObject('', seta);
    seta.PosicaoSeta := posicaoSeta;

    if posicaoSeta in [psDireita, psEsquerda] then
    begin
      seta.Top := shape.Top - 3;
      seta.Left := segmento.X + onde * i;
    end
    else
    begin
      seta.Top := segmento.X + onde * i;
      seta.Left := Shape.Left - 3;
    end;

    seta.Parent := FEntityContainer.EntityArea;
  end;
end;

function TRelationshipArrowPositioner.descobrirSegmentosSemInterseccao(
  relationship1: TEntityRelationship): TSegmentosSemInterseccao;
const
  distanciaDeCadaShape = 5;
var
  vetorExclusao: TArrayOfTPoint; // representa os segmentos com intersecção
  tamVetorExclusao: Integer;

  procedure calcularVetorInclusao(shape: TShape; sentidoSeta: TSentidoSeta; var segmentoSemInterseccao: TArrayOfTPoint);
  var
    k: Integer;
    contVetor: Integer;
    refInicial: Integer;

    procedure incluirSegmentoInclusao(X, Y: Integer);
    begin
      Inc(contVetor);
      SetLength(segmentoSemInterseccao, contVetor);
      segmentoSemInterseccao[contVetor - 1].X := X;
      segmentoSemInterseccao[contVetor - 1].Y := Y;
    end;

  begin
    contVetor := 0;

    if Length(vetorExclusao) = 0 then // nao tem exclusao
    begin
      if sentidoSeta in [psCima, psBaixo] then
        incluirSegmentoInclusao(shape.Top, shape.Top + shape.Height)
      else
        incluirSegmentoInclusao(shape.Left, shape.Left + shape.Width)
    end
    else
    begin
      if sentidoSeta in [psCima, psBaixo] then
        refInicial := shape.Top
      else
        refInicial := shape.Left;

      for k := Low(vetorExclusao) to High(vetorExclusao) do
      begin
        if (vetorExclusao[k].X - refInicial) > 0 then
          incluirSegmentoInclusao(refInicial, vetorExclusao[k].X - 1);

        refInicial := vetorExclusao[k].Y;
      end;

      if sentidoSeta in [psCima, psBaixo] then
      begin
        if ((shape.Top + shape.Height) - vetorExclusao[High(vetorExclusao)].Y) > 0 then
          incluirSegmentoInclusao(vetorExclusao[High(vetorExclusao)].Y + 1, shape.Top + shape.Height);
      end
      else
      begin
        if ((shape.Left + shape.Width) - vetorExclusao[High(vetorExclusao)].Y) > 0 then
          incluirSegmentoInclusao(vetorExclusao[High(vetorExclusao)].Y + 1, shape.Left + shape.Width);
      end;
    end;
  end;

  function fazerMergeSegmentoRetas(X1, Y1, X2, Y2: Integer; var merge: TPoint): Boolean;
  var
    auxXMenor, auxXMaior: TPoint;
  begin
    Result := False;
    //verifica qual ponto X é o menor
    if X1 < X2 then
    begin
      // salva o ponto do X menor
      auxXMenor.X := X1;
      auxXMenor.Y := Y1;
      // salva ponto do X maior
      auxXMaior.X := X2;
      auxXMaior.Y := Y2;
    end
    else
    begin
      // salva o ponto do X menor
      auxXMenor.X := X2;
      auxXMenor.Y := Y2;
      // salva ponto do X maior
      auxXMaior.X := X1;
      auxXMaior.Y := Y1;
    end;

    // verifica se o ponto Y do X menor é >= X do X maior
    if auxXMenor.Y >= auxXMaior.X then // faz o merge
    begin
      // escolhe quem tem o Y maior
      if auxXMenor.Y > auxXMaior.Y then
        merge.Y := auxXMenor.Y
      else
        merge.Y := auxXMaior.Y;

      merge.X := auxXMenor.X;

      Result := True;
    end;
  end;

  procedure incluirSegmentoExclusao(X, Y: Integer);
  var
    k: Integer;
    mergeFeito: Boolean;
    //auxXMenor, auxXMaior: TPoint;
    merge: TPoint;
  begin
    mergeFeito := False;
    // verifica a necessidade de merge de segmento
    for k := Low(vetorExclusao) to High(vetorExclusao) do
    begin
      if fazerMergeSegmentoRetas(vetorExclusao[k].X, vetorExclusao[k].Y, X, Y, merge) then
      begin
        vetorExclusao[k] := merge;
        mergeFeito := True;
        Break;
      end;
    end;

    // se nao fez merge insere um novo segmento
    if not mergeFeito then
    begin
      Inc(tamVetorExclusao);
      SetLength(vetorExclusao, tamVetorExclusao);
      vetorExclusao[tamVetorExclusao -1].X := X;
      vetorExclusao[tamVetorExclusao -1].Y := Y;
    end;
  end;

  procedure ordenarExclusao;
  var
    i, j: Integer;
    aux: TPoint;
  begin
    for i := Low(vetorExclusao) to (High(vetorExclusao) -1) do
    begin
      for j := i + 1 to High(vetorExclusao) do
      begin
        if vetorExclusao[i].X > vetorExclusao[j].X then
        begin
          aux := vetorExclusao[i];
          vetorExclusao[i] := vetorExclusao[j];
          vetorExclusao[j] := aux;
        end;
      end;
    end;
  end;

  procedure analisarUm(shape1, shape2: TShape; sentidoSeta1, sentidoSeta2: TSentidoSeta);
  var
    auxPoint: TPoint;
  begin
    // se nao for o mesmo objeto
    if shape2 <> shape1 then
    begin
      if sentidoSeta1 in [psCima, psBaixo] then // vertical
      begin
        if sentidoSeta2 in [psCima, psBaixo] then // vertical
        begin
          // almoritmo para vertical com vertical
          // dentro da faixa de interseccao vertical
          // verifica em qual lugar
          if Abs(shape2.Left - shape1.Left) < distanciaDeCadaShape then
          begin
            if fazerMergeSegmentoRetas(shape1.Top, shape1.Top + shape1.Height, shape2.Top, shape2.Top + shape2.Height, auxPoint) then
            begin
              // top
              if shape2.Top < shape1.Top then
                auxPoint.X := shape1.Top
              else
                auxPoint.X := shape2.Top;
              // bottom
              if shape2.Top + shape2.Height > shape1.Top + shape1.Height then
                auxPoint.Y := shape1.Top + shape1.Height
              else
                auxPoint.Y := shape2.Top + shape2.Height;

              incluirSegmentoExclusao(auxPoint.X, auxPoint.Y);
            end;
          end;
        end
        else // horizontal
        begin
          // algoritmo para vertical com horizontal
          if (shape1.Left >= shape2.Left) and
             (shape1.Left <= shape2.Left + shape2.Width ) and
             (shape1.Top <= shape2.Top) and
             ((shape1.Top + shape1.Height) >= shape2.Top) then
            incluirSegmentoExclusao(shape2.Top - 4, shape2.Top + 3);
        end;
      end
      else // horizontal
      begin
        if sentidoSeta2 in [psDireita, psEsquerda] then // horizontal
        begin
          // almoritmo para horizontal com horizontal
          // dentro da faixa de interseccao horizontal
          // verifica em qual lugar
          if Abs(shape2.Top - shape1.Top) < distanciaDeCadaShape then
          begin
            if fazerMergeSegmentoRetas(shape1.Left, shape1.Left + shape1.Width, shape2.Left, shape2.Left + shape2.Width, auxPoint) then
            begin
              // left
              if shape2.Left < shape1.Left then
                auxPoint.X := shape1.Left
              else
                auxPoint.X := shape2.Left;
              // rigth
              if shape2.Left + shape2.Width > shape1.Left + shape1.Width then
                auxPoint.Y := shape1.Left + shape1.Width
              else
                auxPoint.Y := shape2.Left + shape2.Width;

              incluirSegmentoExclusao(auxPoint.X, auxPoint.Y);
            end;
          end;
        end
        else // vertical
        begin
          // algoritmo para horizontal com vertical
          if (shape1.Top >= shape2.Top) and
             (shape1.Top <= shape2.Top + shape2.Height ) and
             (shape1.Left <= shape2.Left) and
             ((shape1.Left + shape1.Width) >= shape2.Left) then
            incluirSegmentoExclusao(shape2.Left - 4, shape2.Left + 3);
        end;
      end;
    end;
  end;

  procedure analisarComTodosOsShapes(shape: TShape; sentidoSeta: TSentidoSeta; var segmentoSemInterseccao: TArrayOfTPoint);
  var
    i: Integer;
    relationship2: TEntityRelationship;
  begin
    vetorExclusao := nil;
    tamVetorExclusao := 0;
    // varre todos os objetos
    for i := 0 to FEntityContainer.ListRelationship.Count - 1 do
    begin
      relationship2 := TEntityRelationship(FEntityContainer.ListRelationship.Objects[i]);
      analisarUm(shape, relationship2.ShapeVT, sentidoSeta, relationship2.SentidoVT);
      analisarUm(shape, relationship2.ShapeHR1, sentidoSeta, relationship2.SentidoHR1);
      analisarUm(shape, relationship2.ShapeHR2, sentidoSeta, relationship2.SentidoHR2);
    end;

    // ordernar o vetor de exclusão, primeiro o segmento com posto X menor
    ordenarExclusao;
    calcularVetorInclusao(shape, sentidoSeta, segmentoSemInterseccao);
  end;

begin
  analisarComTodosOsShapes(relationship1.ShapeVT, relationship1.SentidoVT, Result.arrayVT);
  analisarComTodosOsShapes(relationship1.ShapeHR1, relationship1.SentidoHR1, Result.arrayHR1);
  analisarComTodosOsShapes(relationship1.ShapeHR2, relationship1.SentidoHR2, Result.arrayHR2);
end;

destructor TRelationshipArrowPositioner.Destroy;
begin
  DestruirSetas;
  FListArrow.Free;

  inherited Destroy;
end;

procedure TRelationshipArrowPositioner.DestruirSetas;
var
  i: Integer;
begin
  // destroi todas as setas criadas no entity container
  for i := FListArrow.Count - 1 downto 0 do
    FListArrow.Objects[i].Free;

  FListArrow.Clear;
end;

procedure TRelationshipArrowPositioner.PosicionarSetas;
var
  i: Integer;
begin
  DestruirSetas;
  for i := 0 to FEntityContainer.ListRelationship.Count - 1 do
  begin
    analisarPossibilidadeDeSetas(TEntityRelationship(FEntityContainer.ListRelationship.Objects[i]));
  end;
end;

{ TEntityRelationshipDragShape }

constructor TEntityRelationshipDragShape.Create(AOwner: TComponent; EntityRelationship: TEntityRelationship);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  FEntityRelationship := EntityRelationship;
  Cursor := crHSplit;
  OnMouseMove := ShapeMouseMove;
  OnMouseUp := ShapeMouseUp;
  OnMouseDown := ShapeMouseDown;
end;

function TEntityRelationshipDragShape.NormalizarIncremento(X: Integer): Integer;
var
  sinal: Integer;
begin
  if FEntityRelationship.NomeCaminhoUsado = 'ac' then
    sinal := -1
  else
    sinal := 1;

  if x < 0 then
    Result := -1 * sinal
  else if x > 0 then
    Result := 1 * sinal
  else
    Result := 0;
end;

procedure TEntityRelationshipDragShape.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetCapture(Parent.Handle);
    FCapturing := true;
    FMouseDownSpot.X := x;
    FEntityRelationship.EntityContainer.NotificarInicioArrasto;
  end;
end;

procedure TEntityRelationshipDragShape.ShapeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  novaDistancia: Integer;
  antigaDistancia: Integer;
begin
  if FCapturing and (NormalizarIncremento(x) <> 0) then
  begin
    antigaDistancia := FEntityRelationship.DistanciaLateral;
    novaDistancia := FEntityRelationship.DistanciaLateral - (FMouseDownSpot.x - NormalizarIncremento(x));
    FEntityRelationship.DistanciaLateral := novaDistancia;
    // não permite que a margem fique inferior a 2 pixels
    if (Abs(FEntityRelationship.PosiHR1.Width) < DistanciaLateralMinima)
      or (Abs(FEntityRelationship.PosiHR2.Width) < DistanciaLateralMinima + 1) then
      FEntityRelationship.DistanciaLateral := antigaDistancia
    else
      FEntityRelationship.AplicarPosicaoCalculada;
  end;
end;

procedure TEntityRelationshipDragShape.ShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  novaDistancia: Integer;
  antigaDistancia: Integer;
begin
  if FCapturing  then
  begin
    if (NormalizarIncremento(x) <> 0) then
    begin
      ReleaseCapture;
      FCapturing := False;
      antigaDistancia := FEntityRelationship.DistanciaLateral;
      novaDistancia := FEntityRelationship.DistanciaLateral - (FMouseDownSpot.x - NormalizarIncremento(x));
      FEntityRelationship.DistanciaLateral := novaDistancia;
      // não permite que a margem fique inferior a 2 pixels
      if (Abs(FEntityRelationship.PosiHR1.Width) < DistanciaLateralMinima)
        or (Abs(FEntityRelationship.PosiHR2.Width) < DistanciaLateralMinima + 1) then
        FEntityRelationship.DistanciaLateral := antigaDistancia
      else
        FEntityRelationship.AplicarPosicaoCalculada;
    end;
    FEntityRelationship.EntityContainer.NotificarFimArrasto;
  end;
end;

{ TPopMenuEntity }

procedure TPopMenuEntity.AmostraDeDados(Sender: TObject);
var
  OwnerTabela: string;
begin
  OwnerTabela := TEntity(FEntity).SchemaOwner + '.' + TEntity(FEntity).NomeTabela;
  FormPrincipal.DiagramaManager.OpenAmostraContainer(OwnerTabela);
end;

constructor TPopMenuEntity.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // cria o pop menu
  FPopupMenu := TPopupMenu.Create(AOwner);

  //FExibirTodosOsCampos
  FExibirTodosOsCampos := TMenuItem.Create(FPopupMenu);
  FExibirTodosOsCampos.AutoCheck := False;
  FExibirTodosOsCampos.Caption := 'Exibir todos os campos';
  FExibirTodosOsCampos.GroupIndex := 1;
  FExibirTodosOsCampos.OnClick := ExibirTodosOsCampos;

  //FExibirTriggers
  FExibirTriggers := TMenuItem.Create(FPopupMenu);
  FExibirTriggers.Caption := 'Exibir Triggers';
  FExibirTriggers.GroupIndex := 1;
  FExibirTriggers.OnClick := ExibirTriggers;

  //FAmostraDeDados
  FAmostraDeDados := TMenuItem.Create(FPopupMenu);
  FAmostraDeDados.Caption := 'Amostra de Dados';
  FAmostraDeDados.GroupIndex := 1;
  FAmostraDeDados.OnClick := AmostraDeDados;

  //FSeparador1
  FSeparador1 := TMenuItem.Create(FPopupMenu);
  FSeparador1.Name := 'FSeparador1';
  FSeparador1.Caption := '-';
  FSeparador1.GroupIndex := 1;

  //FRemoverDoDiagrama
  FRemoverDoDiagrama := TMenuItem.Create(FPopupMenu);
  FRemoverDoDiagrama.Caption := 'Remover Tabela do Diagrama';
  FRemoverDoDiagrama.GroupIndex := 1;
  FRemoverDoDiagrama.OnClick := RemoverTabelaDoDiagrama;

  //FSeparador2
  FSeparador2 := TMenuItem.Create(FPopupMenu);
  FSeparador2.Caption := '-';
  FSeparador2.GroupIndex := 1;

  //FPesquisaFilhos
  FPesquisaFilhos := TMenuItem.Create(FPopupMenu);
  FPesquisaFilhos.Caption := 'Pesquisar Tabelas Filhas';
  FPesquisaFilhos.OnClick := PesquisarTabelasFilhas;
  FPesquisaFilhos.GroupIndex := 1;

  //FPesquisaPais
  FPesquisaPais := TMenuItem.Create(FPopupMenu);
  FPesquisaPais.Caption := 'Pesquisar Tabelas Pais';
  FPesquisaPais.OnClick := PesquisarTabelasPai;
  FPesquisaPais.GroupIndex := 1;

  // adiciona os itens ao menu
  FPopupMenu.Items.Add(FExibirTodosOsCampos);
  FPopupMenu.Items.Add(FExibirTriggers);
  FPopupMenu.Items.Add(FAmostraDeDados);
  FPopupMenu.Items.Add(FSeparador1);
  FPopupMenu.Items.Add(FRemoverDoDiagrama);
  FPopupMenu.Items.Add(FSeparador2);
  FPopupMenu.Items.Add(FPesquisaFilhos);
  FPopupMenu.Items.Add(FPesquisaPais);
end;

procedure TPopMenuEntity.ExibirTodosOsCampos(Sender: TObject);
begin
  if not TEntity(FEntity).ExibindoTodosOsCampos then
  begin
    TObterMetaDados.ObterCamposNaoChave(TEntity(FEntity));
    FExibirTodosOsCampos.Checked := True;
  end
  else
  begin
    TEntity(FEntity).RemoverColunasNaoChave(False);
    FExibirTodosOsCampos.Checked := False;
  end;
end;

procedure TPopMenuEntity.ExibirTriggers(Sender: TObject);
begin
  TFormVisualizarTriggers.VisualizarTriggers(TEntity(FEntity));
end;

procedure TPopMenuEntity.PesquisarTabelasFilhas(Sender: TObject);
begin
  TFormPesquisarRelacionamentos.ObterTabelasFilhas(TEntity(FEntity));
end;

procedure TPopMenuEntity.PesquisarTabelasPai(Sender: TObject);
begin
  TFormPesquisarRelacionamentos.ObterTabelasPai(TEntity(FEntity));
end;

procedure TPopMenuEntity.PopUp(Entity: TObject);
begin
  FEntity := Entity;
  FExibirTodosOsCampos.Checked := TEntity(FEntity).ExibindoTodosOsCampos;
  FPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TPopMenuEntity.RemoverTabelaDoDiagrama(Sender: TObject);
begin
  if Application.MessageBox('Confirma a remoção da tabela?', 'ATENÇÃO', 
    MB_YESNO + MB_ICONQUESTION) = IDYES then
  begin
    TEntityContainer(TEntity(FEntity).EntityContainer).RemoveEntity(TEntity(FEntity));
  end;
end;


end.
