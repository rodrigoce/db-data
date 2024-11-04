unit uAppFile;

{$mode objfpc}{$H+}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses SysUtils, Dialogs, Generics.Collections, DOM, XMLRead, XMLWrite, uFuncoes;

type

  { Forward Decls }

  TAppFile = class;
  TFileDiagram = class;
  TFileEntity = class;
  TFileRelationship = class;

  { Types }

  { TAppFile }

  TAppFile = class
  private
    FPassword: string;
    FSID: string;
    FTNSPath: string;
    FUserName: string;
    FVersion: string;
    FDiagrams: specialize TObjectList<TFileDiagram>;
    function GetDefaultText(Node: TDOMNode): DOMString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFile(FileName: string);
    procedure SaveFile(FileName: string);
    property Version: string read FVersion write FVersion;
    property TNSPath: string read FTNSPath write FTNSPath;
    property SID: string read FSID write FSID;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Diagrams: specialize TObjectList<TFileDiagram> read FDiagrams write FDiagrams;
  end;

  { TFileDiagram }

  TFileDiagram = class
  private
    FTitle: string;
    FID: string;
    FFileEntities: specialize TObjectList<TFileEntity>;
    FFileRelationships: specialize TObjectList<TFileRelationship>;
  public
    constructor Create(ATitle: string; AID: string);
    destructor Destroy; override;
    property Title: string read FTitle write FTitle;
    property ID: string read FID write FID;
    property Entities: specialize TObjectList<TFileEntity> read FFileEntities write FFileEntities;
    property Relationships: specialize TObjectList<TFileRelationship> read FFileRelationships write FFileRelationships;
  end;

  { TFileEntity }

  TFileEntity = class
  private
    FTop: integer;
    FLeft: integer;
    FOwner: string;
    FTable: string;
    FShowAllColumns: boolean;
  public
    constructor Create(ATop: integer; ALeft: integer; AOwner: string; ATabela: string; ATodosOsCampos: boolean);
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    property Owner: string read FOwner write FOwner;
    property Table: string read FTable write FTable;
    property ShowAllColumns: boolean read FShowAllColumns write FShowAllColumns;
  end;

  { TFileRelationship }

  TFileRelationship = class
  private
    FPathName: string;
    FSideDistance: integer;
    FOwner: string;
    FConstraintName: string;
  public
    constructor Create(ANomeCaminho: string; ADistanciaLateral: integer; AOwner: string; AConstraintName: string);
    property PathName: string read FPathName write FPathName;
    property SideDistance: integer read FSideDistance write FSideDistance;
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

{ TFileDiagram }

constructor TFileDiagram.Create(ATitle: string; AID: string);
begin
  inherited Create;
  FTitle := ATitle;
  FID := AID;
  FFileEntities := specialize TObjectList<TFileEntity>.Create;
  FFileRelationships := specialize TObjectList<TFileRelationship>.Create;
end;

destructor TFileDiagram.Destroy;
begin
  FreeAndNil(FFileEntities);
  FreeAndNil(FFileRelationships);
  inherited;
end;

{ TFileEntity }

constructor TFileEntity.Create(ATop: integer; ALeft: integer; AOwner: string;
  ATabela: string; ATodosOsCampos: boolean);
begin
  inherited Create;
  FTop := ATop;
  FLeft := ALeft;
  FOwner := AOwner;
  FTable := ATabela;
  FShowAllColumns := ATodosOsCampos;
end;

{ TFileRelationship }

constructor TFileRelationship.Create(ANomeCaminho: string;
  ADistanciaLateral: integer; AOwner: string; AConstraintName: string);
begin
  inherited Create;
  FPathName := ANomeCaminho;
  FSideDistance := ADistanciaLateral;
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
  FDiagrams := specialize TObjectList<TFileDiagram>.Create;
end;

destructor TAppFile.Destroy;
begin
  FDiagrams.Free;
  inherited;
end;

procedure TAppFile.OpenFile(FileName: string);
var
  Doc: TXMLDocument;
  fileDiagram: TFileDiagram;
  entity: TFileEntity;
  relationship: TFileRelationship;
  diagramsNode, diagramNode: TDOMNode;
  entitiesNode, entityNode: TDOMNode;
  relationshipsNode, relationshipNode: TDOMNode;
begin
  ReadXMLFile(Doc, FileName);

  diagramsNode := Doc.DocumentElement;
  Version := GetDefaultText(diagramsNode.Attributes.GetNamedItem('versao'));
  TNSPath :=  GetDefaultText(diagramsNode.Attributes.GetNamedItem('tnsPath'));
  SID := GetDefaultText(diagramsNode.Attributes.GetNamedItem('SID'));
  UserName := DeCrypt(GetDefaultText(diagramsNode.Attributes.GetNamedItem('userName')));
  Password := DeCrypt(GetDefaultText(diagramsNode.Attributes.GetNamedItem('password')));

  diagramNode := diagramsNode.FirstChild;

  while Assigned(diagramNode) do
  begin
    fileDiagram := TFileDiagram.Create(
      diagramNode.Attributes.GetNamedItem('titulo').TextContent,
      diagramNode.Attributes.GetNamedItem('id').TextContent);
    FDiagrams.Add(fileDiagram);

    // lê as entidades
    entitiesNode := diagramNode.FindNode('entidades');
    entityNode := entitiesNode.FirstChild;
    while Assigned(entityNode) do
    begin
      entity := TFileEntity.Create(
        StrToInt(entityNode.Attributes.GetNamedItem('top').TextContent),
        StrToInt(entityNode.Attributes.GetNamedItem('left').TextContent),
        entityNode.Attributes.GetNamedItem('owner').TextContent,
        entityNode.Attributes.GetNamedItem('tabela').TextContent,
        StrToBool(entityNode.Attributes.GetNamedItem('todosOsCampos').TextContent));

      fileDiagram.Entities.Add(entity);

      // próxima entity
      entityNode := entityNode.NextSibling;
    end;

    // lê os relacinamentos
    relationshipsNode := diagramNode.FindNode('relacionamentos');
    relationshipNode := relationshipsNode.FirstChild;
    while Assigned(relationshipNode) do
    begin
      relationship := TFileRelationship.Create(
        relationshipNode.Attributes.GetNamedItem('nomeCaminho').TextContent,
        StrToInt(relationshipNode.Attributes.GetNamedItem('distanciaLateral').TextContent),
        relationshipNode.Attributes.GetNamedItem('owner').TextContent,
        relationshipNode.Attributes.GetNamedItem('constraintName').TextContent);

      fileDiagram.Relationships.Add(relationship);

      // próximo relationship
      relationshipNode := relationshipNode.NextSibling;
    end;

    // próximo fileDiagram
    diagramNode := diagramNode.NextSibling;
  end;

  Doc.Free;
end;

procedure TAppFile.SaveFile(FileName: string);
var
  Doc: TXMLDocument;
  fileDiagram: TFileDiagram;
  entity: TFileEntity;
  relationship: TFileRelationship;
  diagramsNode, diagramNode: TDOMNode;
  entitiesNode, entityNode: TDOMNode;
  relationshipsNode, relationshipNode: TDOMNode;
begin
  Doc := TXMLDocument.Create;

  // cria o Document Element
  diagramsNode := Doc.CreateElement('diagramas');
  TDOMElement(diagramsNode).SetAttribute('versao', '1.0');
  TDOMElement(diagramsNode).SetAttribute('tnsPath', TNSPath);
  TDOMElement(diagramsNode).SetAttribute('SID', SID);
  TDOMElement(diagramsNode).SetAttribute('userName', EnCrypt(UserName));
  TDOMElement(diagramsNode).SetAttribute('password', EnCrypt(Password));
  Doc.AppendChild(diagramsNode);

  for fileDiagram in Diagrams do
  begin
    // cria o fileDiagram
    diagramNode := Doc.CreateElement('diagrama');
    TDOMElement(diagramNode).SetAttribute('titulo', fileDiagram.Title);
    TDOMElement(diagramNode).SetAttribute('id', fileDiagram.ID);
    diagramsNode.AppendChild(diagramNode);

    // cria o entidades
    entitiesNode := Doc.CreateElement('entidades');
    diagramNode.AppendChild(entitiesNode);

    for entity in fileDiagram.Entities do
    begin
      // cria o entity
      entityNode := Doc.CreateElement('entidade');
      TDOMElement(entityNode).SetAttribute('top', IntToStr(entity.Top));
      TDOMElement(entityNode).SetAttribute('left', IntToStr(entity.Left));
      TDOMElement(entityNode).SetAttribute('owner', entity.Owner);
      TDOMElement(entityNode).SetAttribute('tabela', entity.Table);
      TDOMElement(entityNode).SetAttribute('todosOsCampos', BoolToStr(entity.ShowAllColumns, True));
      entitiesNode.AppendChild(entityNode);
    end;

    // cria o relacionamentos
    relationshipsNode := Doc.CreateElement('relacionamentos');
    diagramNode.AppendChild(relationshipsNode);

    for relationship in fileDiagram.Relationships do
    begin
      // cria o relationship
      relationshipNode := Doc.CreateElement('relacionamento');
      TDOMElement(relationshipNode).SetAttribute('nomeCaminho', relationship.PathName);
      TDOMElement(relationshipNode).SetAttribute('distanciaLateral', IntToStr(relationship.SideDistance));
      TDOMElement(relationshipNode).SetAttribute('owner', relationship.Owner);
      TDOMElement(relationshipNode).SetAttribute('constraintName', relationship.ConstraintName);
      relationshipsNode.AppendChild(relationshipNode);
    end;
  end;

  WriteXMLFile(Doc, FileName);

  Doc.Free;
end;

end.
