unit uVariaveisGlobais;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 05/11/2013
}

interface
uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Math, ExtCtrls, StdCtrls, IniFiles;

var
  IniFile: TIniFile; // usado para escrever arquivo de INI
  MemoLog: TMemo; // usado para debug
  CacheDeOwners: TStringList; // guarda o cache de ownes do banco de dados para nao precisar repetir a busca que é lenta

implementation

end.
