unit mpegts_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MAX_BUFFER = 102400;
  PKT_SIZE = 188;

{ pids }
  PAT_PID                 = $0000;
  CAT_PID                 = $0001;
  TSDT_PID                = $0002;
  NIT_PID                 = $0010;
  SDT_PID                 = $0011;

  STREAM_TYPE_VIDEO_MPEG1     = $01;
  STREAM_TYPE_VIDEO_MPEG2     = $02;
  STREAM_TYPE_AUDIO_MPEG1     = $03;
  STREAM_TYPE_AUDIO_MPEG2     = $04;
  STREAM_TYPE_PRIVATE_SECTION = $05;
  STREAM_TYPE_PRIVATE_DATA    = $06;
  STREAM_TYPE_AUDIO_AAC       = $0f;
  STREAM_TYPE_AUDIO_AAC_LATM  = $11;
  STREAM_TYPE_VIDEO_MPEG4     = $10;
  STREAM_TYPE_VIDEO_H264      = $1b;
  STREAM_TYPE_VIDEO_VC1       = $ea;
  STREAM_TYPE_VIDEO_DIRAC     = $d1;

  STREAM_TYPE_AUDIO_AC3       = $81;
  STREAM_TYPE_AUDIO_DTS       = $8a;

type
  TMpegTsPid = record
    pid: Integer;
    LastPTS: Int64;
    LastDTS: Int64;
  end;

  TMpegTsAdaptField = record
    Len: Integer;          // 8 Number of bytes in the adaptation field immediately following this byte
    Discont: Boolean;      // 1 Set to 1 if current TS packet is in a discontinuity state
    RandomAccess: Boolean; // 1 Set to 1 if the PES packet in this TS packet starts a video/audio sequence
    EsPrior: Boolean;      // 1 higher priority)
    PcrFlag: Boolean;      // 1 adaptation field does contain a PCR field
    OpcrFlag: Boolean;     // 1 adaptation field does contain an OPCR field
    SpliceFlag: Boolean;   // 1 splice countdown field in adaptation field
    PrivFlag: Boolean;     // 1 private data bytes in adaptation field
    ExtFlag: Boolean;      // 1 adaptation field extension
    PCR: Int64;            // 33+6+9 Program clock reference (90 kHz base)
    PCRExt: Integer;       // PCR extension (27 MHz)
    OPCR: Int64;           // 33+6+9 Original Program clock reference. Helps when one TS is copied into another
    OPCRExt: Integer;      // OPCR extension (27 MHz)
    Splice: Integer;       // 8 Indicates how many TS packets from this one a splicing point occurs (may be negative)
  end;

  TMpegTsPacket = record
    ErrorFlag: Boolean;     // 1  Transport Error Indicator
    PayloadFlag: Boolean;   // 1  Payload Unit Start Indicator
    PriorityFlag: Boolean;  // 1  Transport Priority
    Pid: Integer;           // 13 Packet ID
    Scramble: Integer;      // 2  Transport scrambling control (0,1,2,3)
    AdaptField: Integer;    // 2  Adaptation field control (1,2,3)
    cc: Integer;            // 4  Continuity counter
    af: TMpegTsAdaptField;
  end;

  // PSI (Program Specific Information) structures
  { SI (system information) section }
  TMpegTsSiHeader = record
    SkipBytes: Integer;   // 8 skip bytes to payload
    TableId: Integer;     // 8 table ID
    SyntaxFlag: Boolean;  // 1 section syntax indicator
    PvtFlag: Boolean;     // 1 private indicator
                          // 2 reserved
    Len: Integer;         // 12 section length
    LenPos: Integer;      // .. section length counts from this point
                          // if (syntax_flag == 0), next private data .len-4 bytes
                          // if syntax_flag == 1 then use next fields:
    ExtId: Integer;       // 16 table ID extension
                          // 2 reserved
    Version: Integer;     // 5 version number
    NextFlag: Boolean;    // 1 current next indicator
    Num: Integer;         // 8 section number
    LastNum: Integer;     // 8 last section number
                          // .. private data .len-9 bytes
  end;

  TMpegTsDescriptor = record
    Tag: Integer;         // 8 descriptor tag
    Len: Integer;         // 8 descriptor data length in bytes
    Data: array of Byte;  // raw data
  end;

  { PAT (program association table }
  TMpegTsPatRecord = record
    Num: Integer;     // 16 program num
                      // 3 reserved, '111'
    Pid: Integer;     // 13 packets with this PID are assumed to be PMT tables
  end;

  TMpegTsPat = record
    h: TMpegTsSiHeader;
                        // .h.TableId = 0x00
                        // .h.SectionSyntax = 1
                        // .h.ExtId = transport stream id
    record_count: Integer;   //
    records: array [0..31] of TMpegTsPatRecord;
  end;

  { PMT (program map table) }
  TMpegTsPmtRecord = record
    EsType: Integer;    // 08
                        // 03 reserved '111'
    EsPid: Integer;     // 13
                        // 04 reserved
    DescLen: Integer;   // 12 First two bits must be zero. Entire value may be zero
    DescCount: Integer; //
    desc: array [0..7] of TMpegTsDescriptor;
  end;

  TMpegTsPmt = record
    h: TMpegTsSiHeader;
                        // .h.TableId = 0x02
                        // .h.SectionSyntax = 1
                        // .h.ExtId = program num
                        // ..
                        // 03 reserved
    PcrPid: Integer;    // 13 PID of general timecode stream, or 0x1FFF
                        // 4 reserved
    InfoLen: Integer;   // 12 Sum size of following program descriptors.

    RecordCount: Integer;   //
    records: array [0..31] of TMpegTsPmtRecord;
  end;

  { SDT (Service Description Table) }
  TMpegTsSdtRecord = record
    ServiceId: Integer;   // 16 service ID
                          // 6 reserved
    Schedule: Boolean;    // 1 EIT schedule flag
    Follow: Boolean;      // 1 EIT present following flag
    RunningStatus: Integer; // 3 running status (1-stop, 2-starts, 3-pause, 4-running, 5-off-air)
    FreeCaMode: Boolean;  // 1 free CA mode (0-not scrambled)
    DescrLen: Integer;    // 12 descriptor loop length
    DescrCount: Integer;  // descriptors count
    desc: array [0..7] of TMpegTsDescriptor; // descriptors
  end;

const MAX_SDT_SERVICES = 16;

type
  TMpegTsSdt = record
    h: TMpegTsSiHeader;
                          // .TableId = 0x42
                          // .h.ExtId = transport stream id
    OrigNwId: Integer;    // 16 original network id
                          // 8 reserved
                          // .. services
    ServicesCount: Integer;  // number of services
    services: array [0..MAX_SDT_SERVICES-1] of TMpegTsSdtRecord;
  end;

  { PES (Packetized Elementary Stream }
  TMpegTsPes = record
    StreamId: Integer;
    Len: Integer;            // Can be zero. If the PES packet length is set to zero, the PES packet can be of any length. A value of zero for the PES packet length can be used only when the PES packet payload is a video elementary stream.
    Scrambl: Integer;        // 00 implies not scrambled
    PriorFlag: Boolean;
    AlignFlag: Boolean;      // 1 indicates that the PES packet header is immediately followed by the video start code or audio syncword
    CopyryghtFlag: Boolean;  // 1 implies copyrighted
    OrigFlag: Boolean;       // 1 implies original
    PtsFlag: Boolean;        // 1 PTS present
    DtsFlag: Boolean;        // 1 DTS present
    EscrFlag: Boolean;       // 1 ESCR flag
    EsRateFlag: Boolean;     // 1 ES rate flag
    DsmTrickModeFlag: Boolean; // 1 DSM trick mode flag
    AddCopyInfFlag: Boolean;  // 1 Additional copy info flag
    CrcFlag: Boolean;        // 1 CRC flag
    ExtFlag: Boolean;        // 1 extension flag
    DataLen: Integer;        // 8 PES header data length
                             // ..optional fields
                             // Presentation Time Stamp / Decode Time Stamp
    Pts: Int64;              // 5b 001? 32..30 1 / 29..15 1 / 14..00 1
    Dts: Int64;              // 5b 00?1 32..30 1 / 29..15 1 / 14..00 1
                             // Elementary Stream Clock Reference, used if the stream and system levels are not synchronized
    EscrBase: Int64;         // 6b 00 32..30 1 29..15 1 14..00 1 ext 1
    EscrExt: Integer;
                             // The rate at which data is delivered for this stream, in units of 50 bytes/second
    EsRate: Integer;         // 16 1 es_rate 1
    DsmTrickMode: Integer;
    AddCopyInfo: Integer;
    PrevCrc: Integer;        // 16 The polynomial used is X^16 + X^12 + X^5 + 1
  end;

  TMpegTS = record
    buf: array [0..MAX_BUFFER-1] of Byte;
    Pos: Integer;
    PidsCount: Integer;
    pids: array [0..31] of TMpegTsPid;       // stream pids from PAT/PMT
    cur_pid: ^TMpegTsPid;       // current pid
    LastPcr: Int64;
    cur_pkt: ^TMpegTsPacket;    // current transport packet
  end;


{ Parse stream and dump info to STDOUT }
function ParseStream(AStream: TStream): Boolean;

var
  ts: TMpegTS;
  _verbose: Boolean;

implementation

function GetBit(Val: LongWord; Bit: Integer): Boolean;
begin
  Result := ((Val and (1 shl Bit)) <> 0);
end;

function Get8(var Buf; var Pos: Integer): Byte;
begin
  Result := TByteArray(Buf)[Pos];
  Inc(Pos);
end;

function Get16(var Buf; var Pos: Integer): Word;
begin
  Result := TByteArray(Buf)[Pos+1] + (TByteArray(Buf)[Pos] shl 8);
  Inc(Pos, 2);
end;

function Get32(var Buf; var Pos: Integer): LongWord;
begin
  Result := (TByteArray(Buf)[Pos+3] shl 00)
          + (TByteArray(Buf)[Pos+2] shl 08)
          + (TByteArray(Buf)[Pos+1] shl 16)
          + (TByteArray(Buf)[Pos+0] shl 24);
  Inc(Pos, 4);
end;

function GetStr8(var Buf; var Pos: Integer): string;
var
  len: Integer;
begin
  Result := '';
  len := Get8(Buf, Pos);
  if len > 0 then
  begin
    SetLength(Result, len);
    Move((@Buf + Pos)^, PChar(Result)^, len);
    Inc(Pos, len);
  end;
end;

procedure DumpBits16(n: Word);
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 15 downto 0 do
  begin
    s := s + IntToHex(n shr i, 1);
    if (i mod 4) = 0 then
      s := s + ' ';
  end;
  Write(s);
end;

procedure DumpFlag(Val: Boolean; AText: string);
begin
  if Val then Write(AText);
end;

function GetPidDescription(pid: Integer): string;
begin
  case pid of
    PAT_PID: Result := 'Program Association Table (PAT)';
    CAT_PID: Result := 'Conditional Access Table (CAT)';
    TSDT_PID: Result := 'Transport Stream Description (TSDT)';
    NIT_PID: Result := 'Network Information Table (NIT)';
    SDT_PID: Result := 'Service Description Table (SDT)';
  else
    Result := '';
  end;
end;

function GetPidName(pid: Integer): string;
begin
  case pid of
    $0000: Result := 'PAT';
    $0001: Result := 'CAT';
    $0010: Result := 'NIT,ST';
    $0011: Result := 'SDT,BAT,ST';
    $0012: Result := 'EIT,ST_CIT';
    $0013: Result := 'RST,ST';
    $0014: Result := 'TDT,TOT,ST';
    $0015: Result := 'netwirk synchronization';
    $0016: Result := 'RNT';
    $001c: Result := 'inband signaling';
    $001d: Result := 'measurement';
    $001e: Result := 'DIT';
    $001f: Result := 'SIT';
  else
    Result := '';
  end;
end;

function GetTableIdName(pid: Integer): string;
begin
  case pid of
    $00: Result := 'program association';
    $01: Result := 'conditional access';
    $02: Result := 'program map';
    $03: Result := 'transport stream description';
    $04..$3f: Result := 'reserved';
    $40: Result := 'actual network info';
    $41: Result := 'other network info';
    $42: Result := 'actual service description';
    $46: Result := 'other service description';
    $4a: Result := 'bouquet association';
    $4e: Result := 'actual event info now';
    $4f: Result := 'other event info now';
    $50..$5f: Result := 'event info actual schedule';
    $60..$6f: Result := 'event info other schedule';
    $70: Result := 'time data';
    $71: Result := 'running status';
    $72: Result := 'stuffing';
    $73: Result := 'time offset';
    $74: Result := 'application information';
    $75: Result := 'container';
    $76: Result := 'related content';
    $77: Result := 'content id';
    $78: Result := 'MPE-FEC';
    $79: Result := 'resolution notification';
    $7a: Result := 'MPE-IFEC';
    $7b..$7d: Result := 'reserved';
    $7e: Result := 'discontinuity info';
    $7f: Result := 'selection info';
    $80..$fe: Result := 'user defined';
    $ff: Result := 'reserved';
  else
    Result := 'reserved';
  end;
end;

function GetStreamIdName(StreamId: Integer): string;
begin
  case StreamId of
    $00: Result := 'reserved';
    $01: Result := 'ISO/IEC 11172-2 (MPEG-1 Video)';
    $02: Result := 'ISO/IEC 13818-2 (MPEG-2 Video)';
    $03: Result := 'ISO/IEC 11172-3 (MPEG-1 Audio)';
    $04: Result := 'ISO/IEC 13818-3 (MPEG-2 Audio)';
    $05: Result := 'ISO/IEC 13818-1 (private section)';
    $06: Result := 'ISO/IEC 13818-1 PES';
    $07: Result := 'ISO/IEC 13522 MHEG';
    $08: Result := 'ITU-T H.222.0 annex A DSM-CC';
    $09: Result := 'ITU-T H.222.1';
    $0a: Result := 'ISO/IEC 13818-6 DSM-CC type A';
    $0b: Result := 'ISO/IEC 13818-6 DSM-CC type B';
    $0c: Result := 'ISO/IEC 13818-6 DSM-CC type C';
    $0d: Result := 'ISO/IEC 13818-6 DSM-CC type D';
    $0e: Result := 'ISO/IEC 13818-1 (auxiliary)';
    $0f: Result := 'ISO/IEC 13818-7 (AAC Audio)';
    $10: Result := 'ISO/IEC 14496-2 (MPEG-4 Video)';
    $11: Result := 'ISO/IEC 14496-3 (AAC LATM Audio)';
    $1b: Result := 'ITU-T H.264 (h264 Video)';
    // $ea: Result := '(VC-1 Video)';
    // $d1: Result := '(DIRAC Video)';
    $81: Result := '(AC3 Audio)';
    $8a: Result := '(DTS Audio)';
    $bd: Result := '(non-MPEG Audio, subpictures)';
    $be: Result := '(padding stream)';
    $bf: Result := '(navigation data)';
    $c0..$df: Result := '(AUDIO stream)';
    $e0..$ef: Result := '(VIDEO stream)';
  else
    Result := '';
  end;
end;


function ParseDescriptor(var ABuf: TByteArray; var APos: Integer; var ADesc: TMpegTsDescriptor): Boolean;
var
  s: string;
  tmp8: Byte;
begin
  ADesc.Tag := Get8(ABuf, APos);
  ADesc.Len := Get8(ABuf, APos);

  if (ADesc.Tag = $48) then // service descriptor
  begin
    tmp8 := Get8(ABuf, APos);
    Write('    service type=0x' + IntToHex(tmp8, 2));
    s := GetStr8(ABuf, APos);
    Write(' author="' + s + '"');

    s := GetStr8(ABuf, APos);
    WriteLn(' info="' + s + '"');
  end
  else
  begin
    WriteLn('    descriptor tag=0x' + IntToHex(ADesc.Tag, 2) + ' length=' + IntToStr(ADesc.Len));
  end;
  //ADesc->data = malloc(ADesc->len+1);
  //memcpy(ADesc->data, &ABuf[*APos], ADesc->len);
  Inc(APos, ADesc.Len);

  Result := True;
end;


function ParseSiHeader(var ABuf: TByteArray; var APos: Integer; out h: TMpegTsSiHeader): Boolean;
var
  tmp8: Byte;
  tmp16: Word;
begin
  h.SkipBytes := Get8(ABuf, APos);
  h.TableId := Get8(ABuf, APos); // must be 0x42
  tmp16 := Get16(ABuf, APos);
  h.SyntaxFlag := GetBit(tmp16, 15);
  h.Len := tmp16 and $0fff; // 000 1111
  h.LenPos := APos;
  if h.SyntaxFlag then
  begin
    h.ExtId := Get16(ABuf, APos);
    tmp8 := Get8(ABuf, APos);
    h.NextFlag := GetBit(tmp8, 0);
    h.Version := (tmp8 and $3e) shr 1; // 0011 1110
    h.Num := Get8(ABuf, APos);
    h.LastNum := Get8(ABuf, APos);
  end;

  // dump SI header
  if _verbose then
  begin
    // dump header
    Write(Format('  SI header: skip_bytes=%u table_id=0x%02.2x length=%u', [h.SkipBytes, h.TableId, h.Len]));
    DumpFlag(h.SyntaxFlag, ' syntax');
    if h.SyntaxFlag then
    begin
      Write(Format(' ext_id=%u version=%u number=%u last_number=%u', [h.ExtId, h.Version, h.Num, h.LastNum]));
      DumpFlag(h.NextFlag, ' next');
    end;
    WriteLn();
  end;

  Result := True;
end;


function ParsePat(var ABuf: TByteArray; var APos: Integer; var h: TMpegTsSiHeader): Boolean;
var
  sect_end: Integer;
  pat: TMpegTsPat;
  tmp16: Word;
  pr: ^TMpegTsPatRecord;
begin
  // non-verbose output
  if (not _verbose) then
    WriteLn('pid=0x' + IntToHex(ts.cur_pkt^.Pid, 4) + ' [PAT]');

  pat.h := h;
  pat.record_count := 0;
  sect_end := h.LenPos + h.Len - 4; // section length - CRC32
  while ((APos < PKT_SIZE) and (APos < sect_end)) do
  begin
    pr := @pat.records[pat.record_count];
    Inc(pat.record_count);

    pr^.Num := Get16(ABuf, APos);
    tmp16 := Get16(ABuf, APos);
    pr^.Pid := (tmp16 and $1fff); // 0001 1111

    // dump record info
    WriteLn(Format('  PAT program num=%u pmt_pid=0x%04.4x ', [pr^.Num, pr^.Pid]));
  end;
  Result := True;
end;


function ParsePmt(var ABuf: TByteArray; var APos: Integer; var h: TMpegTsSiHeader): Boolean;
var
  //start_pos: Integer;
  i, i2, end_pos, sect_end: Integer;
  pr: ^TMpegTsPmtRecord;   // single table record
  t: TMpegTsPmt;           // table data
begin
  //start_pos := APos;

  t.RecordCount := 0;
  t.h := h;
  t.PcrPid := Get16(ABuf, APos) and $1fff; // 0001 1111
  t.InfoLen := Get16(ABuf, APos) and $0fff; // 0000 1111

  // non-verbose output
  if (not _verbose) then
    WriteLn('pid=0x' + IntToHex(ts.cur_pkt^.Pid, 4) + ' [PMT]');

  // dump header
  Write(Format('  PMT pcr_pid=0x%04.4x', [t.PcrPid]));
  if (t.InfoLen > 0) then
    Write(Format(' info_len=%u', [t.InfoLen]));
  WriteLn();

  sect_end := t.h.LenPos + t.h.Len - 4; // section length - CRC32
  while ((APos < PKT_SIZE) and (APos < sect_end)) do
  begin
    // parse table record
    pr := @t.records[t.RecordCount];
    Inc(t.RecordCount);

    pr^.DescCount := 0;
    pr^.EsType := Get8(ABuf, APos);
    pr^.EsPid := Get16(ABuf, APos) and $1fff; // 0001 1111
    pr^.DescLen := Get16(ABuf, APos) and $0fff; // 0000 1111

    // add program PID to list
    i2 := 0;
    for i := 0 to ts.PidsCount-1 do
    begin
      if (ts.pids[i].pid = pr^.EsPid) then
      begin
        i2 := 1;
        Break;
      end;
    end;
    if (i2 = 0) then
    begin
      ts.pids[ts.PidsCount].pid := pr^.EsPid;
      Inc(ts.PidsCount);
    end;

    // dump table record info
    Write(Format('  PMT elementary stream type=0x%02.2x pid=0x%04.4x', [pr^.EsType, pr^.EsPid]));
    if (pr^.DescLen > 0) then
      Write(Format(' desc_len=%u', [pr^.DescLen]));
    WriteLn();
    WriteLn('    ' + GetStreamIdName(pr^.EsType));

    if (pr^.DescLen <= (PKT_SIZE - APos)) then
    begin
      // parse descriptors
      pr^.DescCount := 0;
      end_pos := APos + pr^.DescLen;
      while (APos < end_pos) do
      begin
        ParseDescriptor(ABuf, APos, pr^.desc[pr^.DescCount]);
        Inc(pr^.DescCount);
      end;
    end;
  end;
  Result := True;
end;

function ParseSdt(var ABuf: TByteArray; var APos: Integer; var h: TMpegTsSiHeader): Boolean;
var
  //start_pos: Integer;
  end_pos, sect_end: Integer;
  sv: TMpegTsSdtRecord;
  sdt: TMpegTsSdt;
  tmp8: Byte;
  tmp16: Word;
begin
  //start_pos := APos;
  sdt.h := h;
  sdt.OrigNwId := Get16(ABuf, APos);
  tmp8 := Get8(ABuf, APos); // reserved field
  sdt.ServicesCount := 0;

  // non-verbose output
  if (not _verbose) then
    WriteLn(Format('pid=0x%04.4x [SDT]', [ts.cur_pkt^.Pid]));

  // dump header
  WriteLn(Format('  SDT orig_network_id=0x%04.4x ', [sdt.OrigNwId]));

  sect_end := sdt.h.LenPos + sdt.h.Len - 4; // section length - CRC32
  while ((APos < PKT_SIZE) and (APos < sect_end)) do
  begin
    // parse service
    sv.ServiceId := Get16(ABuf, APos);
    tmp8 := Get8(ABuf, APos);
    sv.Schedule := GetBit(tmp8, 1);
    sv.Follow := GetBit(tmp8, 0);

    tmp16 := Get16(ABuf, APos);
    sv.RunningStatus := (tmp16 and $7000) shr 13; // 1110 0000
    sv.FreeCaMode := GetBit(tmp16, 12);
    sv.DescrLen := tmp16 and $0fff; // 0000 1111

    // dump service info
    Write(Format('  SDT service_id=0x%04.4x running=%u descr_len=%u', [sv.ServiceId, sv.RunningStatus, sv.DescrLen]));
    DumpFlag(sv.Schedule, ' schedule');
    DumpFlag(sv.Follow, ' follow');
    DumpFlag(sv.FreeCaMode, ' free_ca_mode');
    WriteLn();

    if (sv.DescrLen <= (PKT_SIZE - APos)) then
    begin
      // parse descriptors
      sv.DescrCount := 0;
      end_pos := APos + sv.DescrLen;
      while (APos < end_pos) do
      begin
        ParseDescriptor(ABuf, APos, sv.desc[sv.DescrCount]);
        Inc(sv.DescrCount);
      end;
    end;

    sdt.services[sdt.ServicesCount] := sv;
    Inc(sdt.ServicesCount);
  end;
  Result := True;
end;


function ParsePesPts(var ABuf: TByteArray; var APos: Integer): Int64;
var
  tmp16: Word;
  tmp8: Byte;
begin
  tmp8 := Get8(ABuf, APos);
  Result := ((tmp8 shr 1) and $07) shl 30;
  tmp16 := Get16(ABuf, APos);
  Result := Result or ((tmp16 shr 1) shl 15);
  tmp16 := Get16(ABuf, APos);
  Result := Result or (tmp16 shr 1);
end;


function ParsePes(var ABuf: TByteArray; var APos: Integer): Boolean;
var
  // PES
  tmp16: Word;
  tmp8: Byte;
  pes: TMpegTsPes;
  //pid: TMpegTsPid;
  ii, payload_pos: Integer;
begin
  // check for PES header
  // Packet start code prefix 3 bytes	0x000001
  tmp16 := Get16(ABuf, APos);
  tmp8 := Get8(ABuf, APos);
  if not ((tmp16 = 0) and (tmp8 = 1)) then
  begin
    // it's not PES, rewind APos backward
    APos := APos - 3;
    Result := False;
    Exit;
  end;

  // PES detected
  tmp8 := Get8(ABuf, APos);
  pes.StreamId := tmp8;
  pes.Len := Get16(ABuf, APos);

  // dump PES header
  if _verbose then
  begin
    Write(Format('  PES stream_id=%#x length=%u  ', [pes.StreamId, pes.Len]));
    WriteLn('  ' + GetStreamIdName(pes.StreamId));
  end
  else
  begin
    // non-verbose output
    WriteLn(Format('pid=0x%04.4x [PES] %s', [ts.cur_pkt^.Pid, GetStreamIdName(pes.StreamId)]));
    // PCR
    if (ts.cur_pkt^.AdaptField <> 0) then
    begin
      if ts.cur_pkt^.af.PcrFlag then
      begin
        Write(Format('  PCR=%u (%i)', [ts.cur_pkt^.af.PCR, (ts.cur_pkt^.af.PCR - ts.LastPcr)]));
        if (ts.cur_pkt^.af.PCRExt <> 0) then
          Write(Format(' pcr_ext=%u', [ts.cur_pkt^.af.PCRExt]));
        WriteLn();
      end;
    end;
  end;

  // optional header
  tmp8 := Get8(ABuf, APos);
  ii := (tmp8 and $c0) shr 6; //1100 0000;
  if (ii = 2) then // bin '10'
  begin
    pes.Scrambl := (tmp8 and $30) shr 4; // 0011 0000
    pes.PriorFlag := GetBit(tmp8, 3);
    pes.AlignFlag := GetBit(tmp8, 2);
    pes.CopyryghtFlag := GetBit(tmp8, 1);
    pes.OrigFlag := GetBit(tmp8, 0);

    tmp8 := Get8(ABuf, APos);
    pes.PtsFlag := GetBit(tmp8, 7);
    pes.DtsFlag := GetBit(tmp8, 6);
    pes.EscrFlag := GetBit(tmp8, 5);
    pes.EsRateFlag := GetBit(tmp8, 4);
    pes.DsmTrickModeFlag := GetBit(tmp8, 3);
    pes.AddCopyInfFlag := GetBit(tmp8, 2);
    pes.CrcFlag := GetBit(tmp8, 1);
    pes.ExtFlag := GetBit(tmp8, 0);
    pes.DataLen := Get8(ABuf, APos);
    payload_pos := APos + pes.DataLen;

    // dump optional header
    if _verbose then
    begin
      Write('  PES flags:');
      DumpFlag((pes.Scrambl <> 0), ' scramble');
      DumpFlag(pes.PtsFlag, ' pts');
      DumpFlag(pes.DtsFlag, ' dts');
      DumpFlag(pes.PriorFlag, ' priority');
      DumpFlag(pes.AlignFlag, ' align');
      DumpFlag(pes.CopyryghtFlag, ' (c)');
      DumpFlag(pes.OrigFlag, ' original');
      DumpFlag(pes.EscrFlag, ' ESCR');
      DumpFlag(pes.EsRateFlag, ' ES_rate');
      DumpFlag(pes.DsmTrickModeFlag, ' DSM_trick_mode');
      DumpFlag(pes.AddCopyInfFlag, ' add_copy_inf');
      DumpFlag(pes.CrcFlag, ' CRC');
      DumpFlag(pes.ExtFlag, ' ext');
      WriteLn();
    end;

    pes.Pts := 0;
    pes.Dts := 0;

    if pes.PtsFlag then // PTS
    begin
      pes.Pts := ParsePesPts(ABuf, APos);
      if (_verbose) then
        Write(Format('  PES PTS=%u', [pes.Pts]))
      else
        Write(Format('  PTS=%u', [pes.Pts]));

      if (ts.cur_pid <> nil) then
      begin
        Write(Format(' (%i)', [(pes.Pts - ts.cur_pid^.LastPTS)]));
        ts.cur_pid^.LastPTS := pes.Pts;
      end;
      if (not _verbose) then
        WriteLn();

      if (pes.DtsFlag) then // DTS
      begin
        pes.Dts := ParsePesPts(ABuf, APos);
        if (_verbose) then
          Write(Format(' DTS=%u', [pes.Dts]))
        else
          Write(Format('  DTS=%u', [pes.Dts]));
        if (ts.cur_pid <> nil) then
        begin
          Write(Format(' (%i)', [(pes.Dts - ts.cur_pid^.LastDTS)]));
          ts.cur_pid^.LastDTS := pes.Dts;
        end;
        if (not _verbose) then
          WriteLn();
      end;
      if (_verbose) then
        WriteLn();
    end;

    // other PES header data
    //...

    // skip to payload
    APos := payload_pos;
  end;

  // PES payload
  if (APos + 4 <= PKT_SIZE) then
  begin
    // dump payload header 4cc
    if (_verbose) then
    begin
      Write(Format('  PES payload 0x%08.8x', [Get32(ABuf, APos)]));
      if (APos + 4 <= PKT_SIZE) then
      begin
        // dump second 4cc
        Write(Format(' 0x%08.8x', [Get32(ABuf, APos)]));
      end;
      WriteLn();
    end;
  end;
  Result := True;
end;


// PCR field value
function ParsePcr(var ABuf: TByteArray; var APos, AExt: Integer): Int64;
var
  // 33 base
  // 06 reserved
  // 09 extension
  tmp8: Byte;
  i: Integer;
begin
  Result := 0;
  for i := 0 to 3 do
  begin
    Result := Result or (Get8(ABuf, APos) shl (8 * (3-i)));
  end;
  tmp8 := Get8(ABuf, APos);
  Result := (Result shl 1) or ((tmp8 and $80) shr 7);

  AExt := ((tmp8 and $01) shl 8) or Get8(ABuf, APos);
end;


function ParsePkt(var ABuf: TByteArray; var pkt: TMpegTsPacket): Boolean;
var
  iPos: Integer;
  i: Integer;
  tmp16: Word;
  tmp8: Byte;
  af: TMpegTsAdaptField;
  sih: TMpegTsSiHeader;
begin
  Result := False;
  iPos := 0;
  af := pkt.af;
  if (ABuf[iPos] <> $47) then
    Exit;

  ts.cur_pkt := @pkt;
  Inc(iPos);
  tmp16 := Get16(ABuf, iPos);
  // dump_16(tmp16); printf(" \n");
  pkt.ErrorFlag := GetBit(tmp16, 15);
  pkt.PayloadFlag := GetBit(tmp16, 14);
  pkt.PriorityFlag := GetBit(tmp16, 13);
  pkt.Pid := tmp16 and $1fff; // 0x1f = 0001 1111
  tmp8 := Get8(ABuf, iPos);
  pkt.Scramble := (tmp8 and $c0) shr 6; // 0xc0 = 1100 0000
  pkt.AdaptField := (tmp8 and $30) shr 4; // 0x30 = 0011 0000
  pkt.cc := tmp8 and $0f; // 0000 1111

  // dump packet
  if (_verbose) then
  begin
    Write(Format('pid=0x%04.4x cc=%u', [pkt.Pid, pkt.cc]));
    DumpFlag(pkt.ErrorFlag, ' error');
    DumpFlag(pkt.PayloadFlag, ' PES/SI');
    DumpFlag(pkt.PriorityFlag, ' priority');
    DumpFlag((pkt.Scramble <> 0), ' scramble');
    DumpFlag((pkt.AdaptField > 1), ' adapt');
    DumpFlag(((pkt.AdaptField and $1) <> 0), ' payload');
    Write('    ' + GetPidDescription(pkt.Pid));
    WriteLn();
  end;

  // adaptation field (optional)
  if (pkt.AdaptField > 1) then
  begin
    af.Len := Get8(ABuf, iPos);

    tmp8 := Get8(ABuf, iPos);
    af.Discont      := GetBit(tmp8, 7);
    af.RandomAccess := GetBit(tmp8, 6);
    af.EsPrior      := GetBit(tmp8, 5);
    af.PcrFlag      := GetBit(tmp8, 4);
    af.OpcrFlag     := GetBit(tmp8, 3);
    af.SpliceFlag   := GetBit(tmp8, 2);
    af.PrivFlag     := GetBit(tmp8, 1);
    af.ExtFlag      := GetBit(tmp8, 0);

    // dump adaptation field
    if (_verbose) then
    begin
      Write(Format('  adapt_field length=%u', [af.Len]));
      DumpFlag(af.Discont, ' discontinuity');
      DumpFlag(af.RandomAccess, ' random_access');
      DumpFlag(af.EsPrior, ' es_priority');
      //DumpFlag(af.PcrFlag, ' PCR');
      //DumpFlag(af.OpcrFlag, ' OPCR');
      //DumpFlag(af.SpliceFlag, ' splice');
      DumpFlag(af.PrivFlag, ' private');
      DumpFlag(af.ExtFlag, ' extension');
    end;

    // optional header data
    if (af.PcrFlag) then
    begin
      ts.LastPcr := af.PCR;
      af.PCR := ParsePcr(ABuf, iPos, af.PCRExt);
      if (_verbose) then
      begin
        Write(Format(' PCR=%u (%i)', [af.PCR, (af.PCR - ts.LastPcr)]));
        if (af.PCRExt <> 0) then
          Write(Format(' pcr_ext=%u', [af.PCRExt]));
      end;
    end;

    if (af.OpcrFlag) then
    begin
      af.OPCR := ParsePcr(ABuf, iPos, af.OPCRExt);
      if (_verbose) then
      begin
        Write(Format(' OPCR=%u', [af.OPCR]));
        if (af.OPCRExt <> 0) then
          Write(Format(' opcr_ext=%u', [af.OPCRExt]));
      end;
    end;

    if (af.SpliceFlag) then
    begin
      af.Splice := Get8(ABuf, iPos);
      if (_verbose) then
        Write(Format(' splice_count=%i', [af.Splice]));
    end;

    if (_verbose) then
      WriteLn();

    iPos := 4 + 1 + af.Len; // skip after adaptation field
  end;

  ts.cur_pid := nil;
  if (pkt.PayloadFlag) then
  begin
    // packet have payload section

    // look for PID in PAT table
    for i := 0 to ts.PidsCount - 1 do
    begin
      if (ts.pids[i].pid = pkt.Pid) then
      begin
        ts.cur_pid := @ts.pids[i];
        // detect PES header
        if not ParsePes(ABuf, iPos) then
          Exit;
      end;
    end;
    if (pkt.Pid > $20) then
    begin
      if (not ParsePes(ABuf, iPos)) then
        Exit;
    end;

    tmp8 := ABuf[iPos + 1]; // table_id
    if (_verbose) then
      WriteLn('  service table name: ' + GetTableIdName(tmp8));

    ParseSiHeader(ABuf, iPos, sih);

    case pkt.Pid of
      $0000: // PAT table
      begin
        ParsePat(ABuf, iPos, sih);
      end;
      $0011: // SDT table
      begin
        ParseSdt(ABuf, iPos, sih);
      end;
    else
      // other packets
      if (tmp8 = $02) then // PMT table
      begin
        ParsePmt(ABuf, iPos, sih);
        Result := True;
        Exit;
      end;

      if (pkt.PayloadFlag) then
      begin
        // unknown payload
        tmp16 := Get16(ABuf, iPos);
        if (_verbose) then
        begin
          Write(Format('  payload header=0x%02.2x  ', [tmp16]));
          DumpBits16(tmp16);
          WriteLn();
        end;
      end;
    end;
  end;

  Result := True;
end;


function ParseStream(AStream: TStream): Boolean;
var
  ir, i: Integer;
  pkt: TMpegTsPacket;
  pkt_buf: TByteArray;
begin
  Result := False;
  if not Assigned(AStream) then Exit;

  ts.LastPcr := 0;
  ts.PidsCount := 0;
  ir := AStream.Read(ts.buf, MAX_BUFFER);
  if (ir <> MAX_BUFFER) then
    Exit;

  i := 0;
  while i < ir do
  begin
    if (ts.buf[i] = $47) then
    begin
      Move(ts.buf[i], pkt_buf, SizeOf(pkt_buf));
      Inc(i, SizeOf(pkt_buf)-1);
      ParsePkt(pkt_buf, pkt);

      Flush(StdOut);
    end;
    Inc(i);
  end;
end;

end.

