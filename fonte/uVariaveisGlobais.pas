unit uVariaveisGlobais;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 05/11/2013
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
