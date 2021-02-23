unit ucComictecaTextList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
  // Comiteca Core units
  uCTKConst,
  // Comiteca Core classes
  ucComictecaText;

type

  cComictecaGenTextList = specialize TFPGObjectList<cComictecaText>;

  { cComictecaTextList }

  cComictecaTextList = class(cComictecaGenTextList)
  public
    procedure LoadFromXML(Parent: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; Parent: TDOMElement); virtual;
  end;

implementation

{ cComictecaTextList }

procedure cComictecaTextList.LoadFromXML(Parent: TDOMElement);
var
  TextNode: TDOMElement;
  TextList: TDOMNodeEnumerator;
  aText: cComictecaText;
  i: integer;
begin
  if not Assigned(Parent) then
    Exit;

  TextList := Parent.GetEnumerator;
  try
    while TextList.MoveNext do
    begin
      if TextList.Current is TDOMElement then
      begin
        TextNode := TDOMElement(TextList.Current);

        if AnsiCompareText(TextNode.TagName, krsCTKXMLText) = 0 then
        begin
          aText := cComictecaText.Create(nil);
          aText.LoadFromXML(TextNode);

          Self.Add(aText);
        end;
      end;
    end;
  finally
    TextList.Free;
  end;
end;

procedure cComictecaTextList.SaveToXML(aXMLDoc: TXMLDocument;
  Parent: TDOMElement);
var
  i: integer;
  XMLText: TDOMElement;
  aText: cComictecaText;
begin
  i := 0;
  while i < Count do
  begin
    aText := Items[i];

    XMLText := aXMLDoc.CreateElement(krsCTKXMLText);
    aText.SaveToXML(aXMLDoc, XMLText);
    Parent.AppendChild(XMLText);
    Inc(i);
  end;
end;

end.

