unit uAppFile;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleotério
Data: 14/10/2013
}

interface

uses SysUtils, Dialogs, Generics.Collections, DOM, XMLRead, XMLWrite, uFuncoes;

type

  { Forward Decls }

  TAppFile = class;
  TDiagrama = class;
  TEntidade = class;
  TRelacionamento = class;

  { Types }

  { TAppFile }

  TAppFile = class
  private
    FPassword: string;
    FSID: string;
    FTNSPath: string;
    FUserName: string;
    FVersao: string;
    FDiagramas: specialize TObjectList<TDiagrama>;
    function GetDefaultText(Node: TDOMNode): DOMString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFile(FileName: string);
    procedure SaveFile(FileName: string);
    property Versao: string read FVersao write FVersao;
    property TNSPath: string read FTNSPath write FTNSPath;
    property SID: string read FSID write FSID;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
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

function IsDBDataFile(fileName: string): boolean;

const
  TargetNamespace = '';

implementation

function IsDBDataFile(fileName: string): boolean;
begin
  Result := False;
  if LowerCase(ExtractFileExt(fileName)) = '.dbdata' then
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

{ TAppFile }

function TAppFile.GetDefaultText(Node: TDOMNode): DOMString;
begin
  if Node = nil then
    Result := ''
  else
    Result := Node.TextContent;
end;

constructor TAppFile.Create;
begin
  inherited;
  FDiagramas := specialize TObjectList<TDiagrama>.Create;
end;

destructor TAppFile.Destroy;
begin
  FDiagramas.Free;
  inherited;
end;

procedure TAppFile.OpenFile(FileName: string);
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
  Versao := GetDefaultText(diagramasNode.Attributes.GetNamedItem('versao'));
  TNSPath :=  GetDefaultText(diagramasNode.Attributes.GetNamedItem('tnsPath'));
  SID := GetDefaultText(diagramasNode.Attributes.GetNamedItem('SID'));
  UserName := DeCrypt(GetDefaultText(diagramasNode.Attributes.GetNamedItem('userName')));
  Password := DeCrypt(GetDefaultText(diagramasNode.Attributes.GetNamedItem('password')));

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

  Doc.Free;
end;

procedure TAppFile.SaveFile(FileName: string);
var
  Doc: TXMLDocument;
  diagrama: TDiagrama;
  entidade: TEntidade;
  relacionamento: TRelacionamento;
  diagramasNode, diagramaNode: TDOMNode;
  entidadesNode, entidadeNode: TDOMNode;
  relacionamentosNode, relacionamentoNode: TDOMNode;
begin
  Doc := TXMLDocument.Create;

  // cria o Document Element
  diagramasNode := Doc.CreateElement('diagramas');
  TDOMElement(diagramasNode).SetAttribute('versao', '1.0');
  TDOMElement(diagramasNode).SetAttribute('tnsPath', TNSPath);
  TDOMElement(diagramasNode).SetAttribute('SID', SID);
  TDOMElement(diagramasNode).SetAttribute('userName', EnCrypt(UserName));
  TDOMElement(diagramasNode).SetAttribute('password', EnCrypt(Password));
  Doc.AppendChild(diagramasNode);

  for diagrama in Diagramas do
  begin
    // cria o diagrama
    diagramaNode := Doc.CreateElement('diagrama');
    TDOMElement(diagramaNode).SetAttribute('titulo', diagrama.Titulo);
    TDOMElement(diagramaNode).SetAttribute('id', diagrama.ID);
    diagramasNode.AppendChild(diagramaNode);

    // cria o entidades
    entidadesNode := Doc.CreateElement('entidades');
    diagramaNode.AppendChild(entidadesNode);

    for entidade in diagrama.Entidades do
    begin
      // cria o entidade
      entidadeNode := Doc.CreateElement('entidade');
      TDOMElement(entidadeNode).SetAttribute('top', IntToStr(entidade.Top));
      TDOMElement(entidadeNode).SetAttribute('left', IntToStr(entidade.Left));
      TDOMElement(entidadeNode).SetAttribute('owner', entidade.Owner);
      TDOMElement(entidadeNode).SetAttribute('tabela', entidade.Tabela);
      TDOMElement(entidadeNode).SetAttribute('todosOsCampos', BoolToStr(entidade.TodosOsCampos, True));
      entidadesNode.AppendChild(entidadeNode);
    end;

    // cria o relacionamentos
    relacionamentosNode := Doc.CreateElement('relacionamentos');
    diagramaNode.AppendChild(relacionamentosNode);

    for relacionamento in diagrama.Relacionamentos do
    begin
      // cria o relacionamento
      relacionamentoNode := Doc.CreateElement('relacionamento');
      TDOMElement(relacionamentoNode).SetAttribute('nomeCaminho', relacionamento.NomeCaminho);
      TDOMElement(relacionamentoNode).SetAttribute('distanciaLateral', IntToStr(relacionamento.DistanciaLateral));
      TDOMElement(relacionamentoNode).SetAttribute('owner', relacionamento.Owner);
      TDOMElement(relacionamentoNode).SetAttribute('constraintName', relacionamento.ConstraintName);
      relacionamentosNode.AppendChild(relacionamentoNode);
    end;
  end;

  WriteXMLFile(Doc, FileName);

  Doc.Free;
end;

end.
