
{**********************************************************************************************}

{                                       XML Data Binding                                       }

{**********************************************************************************************}

unit uDigsERFile;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleotério
Data: 14/10/2013
}

interface

uses SysUtils, Dialogs, Contnrs, Generics.Collections, DOM, XMLRead, XMLWrite;

type

  { Forward Decls }

  TAppFileFormat = class;
  TDiagrama = class;
  TEntidade = class;
  TRelacionamento = class;

  { Types }

  { TAppFileFormat }

  TAppFileFormat = class
  private
    FVersao: string;
    FDiagramas: specialize TObjectList<TDiagrama>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFile(FileName: string);
    property Versao: string read FVersao write FVersao;
    property Diagramas: specialize TObjectList<TDiagrama> read FDiagramas write FDiagramas;
  end;

  { TDiagrama }

  TDiagrama = class
  private
    FTitulo: string;
    FID: string;
    FEntidades: specialize TObjectList<TEntidade>;
    FRelacionamentos: specialize TObjectList<TRelacionamento>;
  public
    constructor Create(ATitle: string; AID: string);
    destructor Destroy; override;
    property Titulo: string read FTitulo write FTitulo;
    property ID: string read FID write FID;
    property Entidades: specialize TObjectList<TEntidade> read FEntidades write FEntidades;
    property Relacionamentos: specialize TObjectList<TRelacionamento> read FRelacionamentos write FRelacionamentos;
  end;

  { TEntidade }

  TEntidade = class
  private
    FTop: integer;
    FLeft: integer;
    FOwner: string;
    FTabela: string;
    FTodosOsCampos: boolean;
  public
    constructor Create(ATop: integer; ALeft: integer; AOwner: string; ATabela: string; ATodosOsCampos: boolean);
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    property Owner: string read FOwner write FOwner;
    property Tabela: string read FTabela write FTabela;
    property TodosOsCampos: boolean read FTodosOsCampos write FTodosOsCampos;
  end;

  { TRelacionamento }

  TRelacionamento = class
  private
    FNomeCaminho: string;
    FDistanciaLateral: integer;
    FOwner: string;
    FConstraintName: string;
  public
    constructor Create(ANomeCaminho: string; ADistanciaLateral: integer; AOwner: string; AConstraintName: string);
    property NomeCaminho: string read FNomeCaminho write FNomeCaminho;
    property DistanciaLateral: integer read FDistanciaLateral write FDistanciaLateral;
    property Owner: string read FOwner write FOwner;
    property ConstraintName: string read FConstraintName write FConstraintName;
  end;


  { Global Functions }

(*function Getdiagramas(Doc: IXMLDocument): IXMLDiagramasType;
function Loaddiagramas(const FileName: WideString): IXMLDiagramasType;
function Newdiagramas: IXMLDiagramasType;        *)
function IsDigsERFile(fileName: string): boolean;

const
  TargetNamespace = '';

implementation

{ Global Functions }

(*function Getdiagramas(Doc: IXMLDocument): IXMLDiagramasType;
begin
  Result := Doc.GetDocBinding('diagramas', TXMLDiagramasType, TargetNamespace) as IXMLDiagramasType;
end;

function Loaddiagramas(const FileName: WideString): IXMLDiagramasType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('diagramas', TXMLDiagramasType, TargetNamespace) as IXMLDiagramasType;
end;

function Newdiagramas: IXMLDiagramasType;
begin
  Result := NewXMLDocument.GetDocBinding('diagramas', TXMLDiagramasType, TargetNamespace) as IXMLDiagramasType;
end; *)

function IsDigsERFile(fileName: string): boolean;
begin
  Result := False;
  if LowerCase(ExtractFileExt(fileName)) = '.dger' then
    if FileExists(fileName) then
      Result := True;
end;

{ TDiagrama }

constructor TDiagrama.Create(ATitle: string; AID: string);
begin
  inherited Create;
  FTitulo := ATitle;
  FID := AID;
  FEntidades := specialize TObjectList<TEntidade>.Create;
  FRelacionamentos := specialize TObjectList<TRelacionamento>.Create;
end;

destructor TDiagrama.Destroy;
begin
  FreeAndNil(FEntidades);
  FreeAndNil(FRelacionamentos);
  inherited;
end;

{ TEntidade }

constructor TEntidade.Create(ATop: integer; ALeft: integer; AOwner: string;
  ATabela: string; ATodosOsCampos: boolean);
begin
  inherited Create;
  FTop := ATop;
  FLeft := ALeft;
  FOwner := AOwner;
  FTabela := ATabela;
  FTodosOsCampos := ATodosOsCampos;
end;

{ TRelacionamento }

constructor TRelacionamento.Create(ANomeCaminho: string;
  ADistanciaLateral: integer; AOwner: string; AConstraintName: string);
begin
  inherited Create;
  FNomeCaminho := ANomeCaminho;
  FDistanciaLateral := ADistanciaLateral;
  FOwner := AOwner;
  FConstraintName := AConstraintName;
end;

{ TAppFileFormat }

constructor TAppFileFormat.Create;
begin
  inherited;
  FDiagramas := specialize TObjectList<TDiagrama>.Create;
end;

destructor TAppFileFormat.Destroy;
begin
  FDiagramas.Free;
  inherited;
end;

procedure TAppFileFormat.OpenFile(FileName: string);
var
  Doc: TXMLDocument;
  diagrama: TDiagrama;
  entidade: TEntidade;
  relacionamento: TRelacionamento;
  diagramasNode, diagramaNode: TDOMNode;
  entidadesNode, entidadeNode: TDOMNode;
  relacionamentosNode, relacionamentoNode: TDOMNode;
begin
  ReadXMLFile(Doc, FileName);

  diagramasNode := Doc.DocumentElement;
  Versao := diagramasNode.Attributes.GetNamedItem('versao').TextContent;

  diagramaNode := diagramasNode.FirstChild;

  while Assigned(diagramaNode) do
  begin
    diagrama := TDiagrama.Create(
      diagramaNode.Attributes.GetNamedItem('titulo').TextContent,
      diagramaNode.Attributes.GetNamedItem('id').TextContent);
    FDiagramas.Add(diagrama);

    // lê as entidades
    entidadesNode := diagramaNode.FindNode('entidades');
    entidadeNode := entidadesNode.FirstChild;
    while Assigned(entidadeNode) do
    begin
      entidade := TEntidade.Create(
        StrToInt(entidadeNode.Attributes.GetNamedItem('top').TextContent),
        StrToInt(entidadeNode.Attributes.GetNamedItem('left').TextContent),
        entidadeNode.Attributes.GetNamedItem('owner').TextContent,
        entidadeNode.Attributes.GetNamedItem('tabela').TextContent,
        StrToBool(entidadeNode.Attributes.GetNamedItem('todosOsCampos').TextContent));

      diagrama.Entidades.Add(entidade);

      // próxima entidade
      entidadeNode := entidadeNode.NextSibling;
    end;

    // lê os relacinamentos
    relacionamentosNode := diagramaNode.FindNode('relacionamentos');
    relacionamentoNode := relacionamentosNode.FirstChild;
    while Assigned(relacionamentoNode) do
    begin
      relacionamento := TRelacionamento.Create(
        relacionamentoNode.Attributes.GetNamedItem('nomeCaminho').TextContent,
        StrToInt(relacionamentoNode.Attributes.GetNamedItem('distanciaLateral').TextContent),
        relacionamentoNode.Attributes.GetNamedItem('owner').TextContent,
        relacionamentoNode.Attributes.GetNamedItem('constraintName').TextContent);

      diagrama.Relacionamentos.Add(relacionamento);

      // próximo relacionamento
      relacionamentoNode := relacionamentoNode.NextSibling;
    end;

    // próximo diagrama
    diagramaNode := diagramaNode.NextSibling;
  end;

  // Libera o documento da memória
  Doc.Free;
end;

end.
