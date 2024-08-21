unit uVariaveisGlobais;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface
uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls,
  ExtCtrls, StdCtrls, uAppFile{, IniFiles};

var
  AppFile: TAppFile;
  MemoLog: TMemo; // usado para debug
  CacheDeOwners: TStringList; // guarda o cache de ownes do banco de dados para nao precisar repetir a busca que é lenta
  {IniFile: TIniFile;}

implementation

end.
