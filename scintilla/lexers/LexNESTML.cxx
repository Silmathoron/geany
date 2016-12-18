/** @file LexNESTML.cxx
 ** Lexer for NESTML.
 **
 ** Copyright (c) 2013 by SiegeLord <slabode@aim.com>
 ** Converted to lexer object and added further folding features/properties by "Udo Lechner" <dlchnr(at)gmx(dot)net>
 **/
// Copyright 1998-2005 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>

#include <string>
#include <vector>
#include <map>

#include "ILexer.h"
#include "Scintilla.h"
#include "SciLexer.h"

#include "PropSetSimple.h"
#include "WordList.h"
#include "LexAccessor.h"
#include "Accessor.h"
#include "StyleContext.h"
#include "CharacterSet.h"
#include "LexerModule.h"
#include "OptionSet.h"

#ifdef SCI_NAMESPACE
using namespace Scintilla;
#endif


inline bool IsAWordChar(int ch) {
   return (ch < 0x80) && (isalnum(ch) || ch == '_' || ch == '$' || ch == '\'');
}

inline size_t lengthOf(const char** charArray) {
   return (sizeof(*charArray)/sizeof(**charArray));
}

// keywords related declarations
static const int NUM_NESTML_KEYWORD_LISTS = 4;
static const int MAX_NESTML_IDENT_CHARS = 1023;

enum kwType { kwNeuron, kwDef, kwOther };

// units
const char *units[] = {"m", "g", "s", "A", "K", "mol", "cd", "Bq",
   "Hz", "Pa", "V", "C", "J", "S", "W", "F", "N", "Sv", "Wb", "Gy", "Ohm",
   "TH", "kat"};
std::vector<std::string> vecUnits(units, units + 23);

const char *unitMagnitudes[] = {"y", "z", "a", "f", "p", "n", "mu", "m", "c",
   "d", "da", "h", "k", "M", "G", "T", "P", "E", "Z", "Y"};
std::vector<std::string> vecMagnitudes(
   unitMagnitudes, unitMagnitudes + 20);

bool MatchUnit(Accessor &styler, Sci_Position pos, unsigned int mag_len)
{
   // test all units
   for (unsigned int j = 0; j < vecUnits.size(); j++)
   {
      size_t k = 0;
      size_t unit_len = vecUnits[j].size();
      char currentChar = styler.SafeGetCharAt(pos + mag_len + k - 2);
      printf(&currentChar);
      printf("\n");
      bool matchString = true;
      while ((currentChar != '\0') && IsAWordChar(currentChar) && (k < unit_len))
      {
         printf("Compare: ");
         printf(&currentChar);
         printf(" ");
         printf(&vecUnits[j][k]);
         printf("\n");
         matchString = currentChar == vecUnits[j][k] ? matchString : false;
         k++;
         currentChar = styler.SafeGetCharAt(pos + mag_len + k - 2);
      }
      // if the current unit matched, check is not part of a longer word
      if (matchString) {
         printf("string matched: ");
         printf(&currentChar);
         printf("\n");
         if (!IsAWordChar(styler.SafeGetCharAt(pos + mag_len + k - 1))) {
            return true;
         }
      }
   }
   return false;
}

bool IsNestmlUnit(Accessor &styler, const char* s, Sci_Position pos)
{
   bool isUnit = false;
   size_t mag_len = 0;
   for (size_t i = 0; i < vecMagnitudes.size(); i++)
   {
      char magnitude[vecMagnitudes[i].size()];
      strcpy(magnitude, vecMagnitudes[i].c_str());
      //~ printf(magnitude);
      //~ printf("\n");
      // test whether we have a magnitude
      const char *resSearch = strstr(s, magnitude);
      if (resSearch != NULL && !(resSearch-s)) {
         mag_len = vecMagnitudes[i].size();
         char currentChar = styler.SafeGetCharAt(pos + mag_len - 2);
         // if we do, check whether it is folloed by a valid unit
         printf("matching magnitude: ");
         printf(resSearch);
         printf(" ");
         printf(magnitude);
         printf(" ");
         printf(&currentChar);
         printf("\n");
         isUnit = MatchUnit(styler, pos, mag_len);
      } else {
         mag_len = 0;
      }
   }
   if (!isUnit)
   {
      printf("no match magnitude\n");
      isUnit = MatchUnit(styler, pos, mag_len);
   }
   return isUnit;
}

// comments
static bool IsStreamCommentStyle(int style) {
	return (style == SCE_NESTML_COMMENTBLOCK || style == SCE_NESTML_COMMENTLINE);
}

// Options used for LexerNESTML
struct OptionsNESTML {
	bool fold;
	bool foldSyntaxBased;
	bool foldComment;
	bool foldCommentMultiline;
	bool foldCommentExplicit;
	std::string foldExplicitStart;
	std::string foldExplicitEnd;
	bool foldExplicitAnywhere;
	bool foldCompact;
	int  foldAtElseInt;
	bool foldAtElse;
	OptionsNESTML() {
		fold = false;
		foldSyntaxBased = true;
		foldComment = false;
		foldCommentMultiline = true;
		foldCommentExplicit = true;
		foldExplicitStart = "";
		foldExplicitEnd   = "";
		foldExplicitAnywhere = false;
		foldCompact = true;
		foldAtElseInt = -1;
		foldAtElse = false;
	}
};

static const char * const NESTMLWordLists[NUM_NESTML_KEYWORD_LISTS + 1] = {
			"Primary keywords and identifiers",
         "Secondary keywords",
			"Units and types",
			"Other keywords",
			0,
		};

struct OptionSetNESTML : public OptionSet<OptionsNESTML> {
	OptionSetNESTML() {
		DefineProperty("fold", &OptionsNESTML::fold);

		DefineProperty("fold.comment", &OptionsNESTML::foldComment);

		DefineProperty("fold.compact", &OptionsNESTML::foldCompact);

		DefineProperty("fold.at.else", &OptionsNESTML::foldAtElse);

		DefineProperty("fold.NESTML.syntax.based", &OptionsNESTML::foldSyntaxBased,
			"Set this property to 0 to disable syntax based folding.");

		DefineProperty("fold.NESTML.comment.multiline", &OptionsNESTML::foldCommentMultiline,
			"Set this property to 0 to disable folding multi-line comments when fold.comment=1.");

		DefineProperty("fold.NESTML.comment.explicit", &OptionsNESTML::foldCommentExplicit,
			"Set this property to 0 to disable folding explicit fold points when fold.comment=1.");

		DefineProperty("fold.NESTML.explicit.start", &OptionsNESTML::foldExplicitStart,
			"The string to use for explicit fold start points, replacing the standard //{.");

		DefineProperty("fold.NESTML.explicit.end", &OptionsNESTML::foldExplicitEnd,
			"The string to use for explicit fold end points, replacing the standard //}.");

		DefineProperty("fold.NESTML.explicit.anywhere", &OptionsNESTML::foldExplicitAnywhere,
			"Set this property to 1 to enable explicit fold points anywhere, not just in line comments.");

		DefineProperty("lexer.NESTML.fold.at.else", &OptionsNESTML::foldAtElseInt,
			"This option enables NESTML folding on a \"} else {\" line of an if statement.");

		DefineWordListSets(NESTMLWordLists);
	}
};

class LexerNESTML : public ILexer {
	WordList keywords[NUM_NESTML_KEYWORD_LISTS];
	OptionsNESTML options;
	OptionSetNESTML osNESTML;
public:
	virtual ~LexerNESTML() {
	}
	void SCI_METHOD Release() {
		delete this;
	}
	int SCI_METHOD Version() const {
		return lvOriginal;
	}
	const char * SCI_METHOD PropertyNames() {
		return osNESTML.PropertyNames();
	}
	int SCI_METHOD PropertyType(const char *name) {
		return osNESTML.PropertyType(name);
	}
	const char * SCI_METHOD DescribeProperty(const char *name) {
		return osNESTML.DescribeProperty(name);
	}
	Sci_Position SCI_METHOD PropertySet(const char *key, const char *val);
	const char * SCI_METHOD DescribeWordListSets() {
		return osNESTML.DescribeWordListSets();
	}
	Sci_Position SCI_METHOD WordListSet(int n, const char *wl);
	void SCI_METHOD Lex(Sci_PositionU startPos, Sci_Position length, int initStyle, IDocument *pAccess);
	void SCI_METHOD Fold(Sci_PositionU startPos, Sci_Position length, int initStyle, IDocument *pAccess);
	void * SCI_METHOD PrivateCall(int, void *) {
		return 0;
	}
	static ILexer *LexerFactoryNESTML() {
		return new LexerNESTML();
	}
};

Sci_Position SCI_METHOD LexerNESTML::PropertySet(const char *key, const char *val) {
	if (osNESTML.PropertySet(&options, key, val)) {
		return 0;
	}
	return -1;
}

Sci_Position SCI_METHOD LexerNESTML::WordListSet(int n, const char *wl) {
	Sci_Position firstModification = -1;
	if (n < NUM_NESTML_KEYWORD_LISTS) {
		WordList *wordListN = &keywords[n];
		WordList wlNew;
		wlNew.Set(wl);
		if (*wordListN != wlNew) {
			wordListN->Set(wl);
			firstModification = 0;
		}
	}
	return firstModification;
}

static bool IsWhitespace(int c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

/* This isn't quite right for Unicode identifiers */
static bool IsWordStart(int ch) {
	return (ch < 0x80) && (isalpha(ch) || ch == '_' || ch == '$');
}

static void ScanWhitespace(Accessor& styler, Sci_Position& pos, Sci_Position max) {
	while (IsWhitespace(styler.SafeGetCharAt(pos, '\0')) && pos < max) {
		if (pos == styler.LineEnd(styler.GetLine(pos)))
			styler.SetLineState(styler.GetLine(pos), 0);
		pos++;
	}
	styler.ColourTo(pos-1, SCE_NESTML_DEFAULT);
}

static void GrabString(char* s, Accessor& styler, Sci_Position start, Sci_Position len) {
	for (Sci_Position ii = 0; ii < len; ii++)
		s[ii] = styler[ii + start];
	s[len] = '\0';
}

static void ScanIdentifier(Accessor& styler, StyleContext& sc, Sci_Position& pos, WordList *keywords, kwType& kwLast) {
	Sci_Position start = pos;
	while (IsAWordChar(styler.SafeGetCharAt(pos, '\0')))
   {
		//~ sc.Forward();
      pos++;
   }

   char s[MAX_NESTML_IDENT_CHARS + 1];
   int len = pos - start;
   len = len > MAX_NESTML_IDENT_CHARS ? MAX_NESTML_IDENT_CHARS : len;
   GrabString(s, styler, start, len);
   printf("String:\n");
   printf(s);
   printf("\n");
   // check for keywords
   bool keyword = false;
   for (int ii = 0; ii < NUM_NESTML_KEYWORD_LISTS; ii++) {
      if (keywords[ii].InList(s)) {
         styler.ColourTo(pos - 1, SCE_NESTML_WORD + ii);
         keyword = true;
         break;
      }
   }
   if (keyword)
   {
      printf("keyword\n");
      char *resSearch;
      resSearch = strstr(s, "neuron");
      if (resSearch != NULL)
         kwLast = kwNeuron;
      else
      {
         bool isDef = false;
         const char *defKeywords[] = {"alias", "shape", "function"};
         std::vector<std::string> vecDefKw(
            defKeywords, defKeywords + 3);
         for (size_t jj=0; jj<vecDefKw.size(); jj++)
         {
            char defKw[vecDefKw[jj].size()];
            strcpy(defKw, vecDefKw[jj].c_str());
            resSearch = strstr(s, defKw);
            if (resSearch != NULL)
               isDef = true;
         }
         if (isDef)
            kwLast = kwDef;
         else
            kwLast = kwOther;
      }
   }
   else {
      // if not a keyword, check if neuron/def identifier
      if (kwLast == kwNeuron)
      {
         printf("neuron name!\n");
         kwLast = kwOther;
         styler.ColourTo(pos - 1, SCE_NESTML_NEURON);
      }
      else if (kwLast == kwDef)
      {
         kwLast = kwOther;
         printf("defname!\n");
         styler.ColourTo(pos - 1, SCE_NESTML_DEFNAME);
      }
      else
      {
         // check for units
         if (IsNestmlUnit(styler, s, pos))
         {
            printf("unit!\n");
            kwLast = kwOther;
            styler.ColourTo(pos - 1, SCE_NESTML_UNIT);
         }
         else
         {
            kwLast = kwOther;
            styler.ColourTo(pos - 1, SCE_NESTML_DEFAULT);
         }
      }
   }
}

/* Scans a sequence of digits, returning true if it found any. */
static bool ScanDigits(Accessor& styler, Sci_Position& pos, int base) {
	Sci_Position old_pos = pos;
	for (;;) {
		int c = styler.SafeGetCharAt(pos, '\0');
		if (IsADigit(c, base) || c == '_')
			pos++;
		else
			break;
	}
	return old_pos != pos;
}

/* Scans an integer and floating point literals. */
static void ScanNumber(Accessor& styler, Sci_Position& pos) {
	int base = 10;
	int c = styler.SafeGetCharAt(pos, '\0');
	int n = styler.SafeGetCharAt(pos + 1, '\0');
	bool error = false;
	/* Scan the prefix, thus determining the base.
	 * 10 is default if there's no prefix. */
	if (c == '0' && n == 'x') {
		pos += 2;
		base = 16;
	} else if (c == '0' && n == 'b') {
		pos += 2;
		base = 2;
	} else if (c == '0' && n == 'o') {
		pos += 2;
		base = 8;
	}

	/* Scan initial digits. The literal is malformed if there are none. */
	error |= !ScanDigits(styler, pos, base);
	/* See if there's an integer suffix. We mimic the NESTML's lexer
	 * and munch it even if there was an error above. */
	c = styler.SafeGetCharAt(pos, '\0');
	if (c == 'u' || c == 'i') {
		pos++;
		c = styler.SafeGetCharAt(pos, '\0');
		n = styler.SafeGetCharAt(pos + 1, '\0');
		if (c == '8' || c == 's') {
			pos++;
		} else if (c == '1' && n == '6') {
			pos += 2;
		} else if (c == '3' && n == '2') {
			pos += 2;
		} else if (c == '6' && n == '4') {
			pos += 2;
		} else {
			error = true;
		}
	/* See if it's a floating point literal. These literals have to be base 10.
	 */
	} else if (!error) {
		/* If there's a period, it's a floating point literal unless it's
		 * followed by an identifier (meaning this is a method call, e.g.
		 * `1.foo()`) or another period, in which case it's a range (e.g. 1..2)
		 */
		n = styler.SafeGetCharAt(pos + 1, '\0');
		if (c == '.' && !(IsWordStart(n) || n == '.')) {
			error |= base != 10;
			pos++;
			/* It's ok to have no digits after the period. */
			ScanDigits(styler, pos, 10);
		}

		/* Look for the exponentiation. */
		c = styler.SafeGetCharAt(pos, '\0');
		if (c == 'e' || c == 'E') {
			error |= base != 10;
			pos++;
			c = styler.SafeGetCharAt(pos, '\0');
			if (c == '-' || c == '+')
				pos++;
			/* It is invalid to have no digits in the exponent. */
			error |= !ScanDigits(styler, pos, 10);
		}

		/* Scan the floating point suffix. */
		c = styler.SafeGetCharAt(pos, '\0');
		if (c == 'f') {
			error |= base != 10;
			pos++;
			c = styler.SafeGetCharAt(pos, '\0');
			n = styler.SafeGetCharAt(pos + 1, '\0');
			if (c == '3' && n == '2') {
				pos += 2;
			} else if (c == '6' && n == '4') {
				pos += 2;
			} else {
				error = true;
			}
		}
	}

	if (error)
		styler.ColourTo(pos - 1, SCE_NESTML_LEXERROR);
	else
		styler.ColourTo(pos - 1, SCE_NESTML_NUMBER);
}

static bool IsOneCharOperator(int c) {
	return c == ';' || c == ',' || c == '(' || c == ')'
	    || c == '{' || c == '}' || c == '[' || c == ']'
	    || c == '@' || c == '#' || c == '~' || c == '+'
	    || c == '*' || c == '/' || c == '^' || c == '%'
	    || c == '.' || c == ':' || c == '!' || c == '<'
	    || c == '>' || c == '=' || c == '-' || c == '&'
	    || c == '|' || c == '$' || c == '?';
}

static bool IsTwoCharOperator(int c, int n) {
	return (c == '.' && n == '.') || (c == ':' && n == ':')
	    || (c == '!' && n == '=') || (c == '<' && n == '<')
	    || (c == '<' && n == '=') || (c == '>' && n == '>')
	    || (c == '>' && n == '=') || (c == '=' && n == '=')
	    || (c == '=' && n == '>') || (c == '-' && n == '>')
	    || (c == '&' && n == '&') || (c == '|' && n == '|')
	    || (c == '-' && n == '=') || (c == '&' && n == '=')
	    || (c == '|' && n == '=') || (c == '+' && n == '=')
	    || (c == '*' && n == '=') || (c == '/' && n == '=')
	    || (c == '^' && n == '=') || (c == '%' && n == '=');
}

static bool IsThreeCharOperator(int c, int n, int n2) {
	return (c == '<' && n == '<' && n2 == '=')
	    || (c == '>' && n == '>' && n2 == '=');
}

static bool IsValidCharacterEscape(int c) {
	return c == 'n'  || c == 'r' || c == 't' || c == '\\'
	    || c == '\'' || c == '"' || c == '0';
}

static bool IsValidStringEscape(int c) {
	return IsValidCharacterEscape(c) || c == '\n' || c == '\r';
}

static bool ScanNumericEscape(Accessor &styler, Sci_Position& pos, Sci_Position num_digits, bool stop_asap) {
	for (;;) {
		int c = styler.SafeGetCharAt(pos, '\0');
		if (!IsADigit(c, 16))
			break;
		num_digits--;
		pos++;
		if (num_digits == 0 && stop_asap)
			return true;
	}
	if (num_digits == 0) {
		return true;
	} else {
		return false;
	}
}

static void ResumeBlockComment(Accessor &styler, Sci_Position& pos, Sci_Position max, int level) {
	int c = styler.SafeGetCharAt(pos, '\0');
   
	for (;;) {
		int n = styler.SafeGetCharAt(pos + 1, '\0');
		if (pos == styler.LineEnd(styler.GetLine(pos)))
			styler.SetLineState(styler.GetLine(pos), level);
		if (c == '*') {
			pos++;
			if (n == '/') {
				pos++;
				level--;
				if (level == 0) {
					styler.SetLineState(styler.GetLine(pos), 0);
					styler.ColourTo(pos - 1, SCE_NESTML_COMMENTBLOCK);
					break;
				}
			}
		} else if (c == '/') {
			pos++;
			if (n == '*') {
				pos++;
				level++;
			}
		}
		else {
			pos++;
		}
		if (pos >= max) {
			styler.ColourTo(pos - 1, SCE_NESTML_COMMENTBLOCK);
			break;
		}
		c = styler.SafeGetCharAt(pos, '\0');
	}
}

static void ResumeLineComment(Accessor &styler, Sci_Position& pos, Sci_Position max) {
	int c = styler.SafeGetCharAt(pos, '\0');
   
	while (pos < max && c != '\n') {
		if (pos == styler.LineEnd(styler.GetLine(pos)))
			styler.SetLineState(styler.GetLine(pos), 0);
		pos++;
		c = styler.SafeGetCharAt(pos, '\0');
	}

   styler.ColourTo(pos - 1, SCE_NESTML_COMMENTLINE);
}

static void ScanComments(Accessor &styler, Sci_Position& pos, Sci_Position max) {
	pos++;
	int c = styler.SafeGetCharAt(pos, '\0');
	pos++;
	if (c == '*')
		ResumeBlockComment(styler, pos, max, 1);
   else
      ResumeLineComment(styler, pos, max);
}

static void ResumeString(Accessor &styler, Sci_Position& pos, Sci_Position max) {
	int c = styler.SafeGetCharAt(pos, '\0');
	bool error = false;
	while (c != '"' && !error) {
		if (pos >= max) {
			error = true;
			break;
		}
		if (pos == styler.LineEnd(styler.GetLine(pos)))
			styler.SetLineState(styler.GetLine(pos), 0);

		c = styler.SafeGetCharAt(pos, '\0');
	}
	pos++;
	styler.ColourTo(pos - 1, SCE_NESTML_STRING);
}

void SCI_METHOD LexerNESTML::Lex(Sci_PositionU startPos, Sci_Position length, int initStyle, IDocument *pAccess) {
	PropSetSimple props;
	Accessor styler(pAccess, &props);
	Sci_Position pos = startPos;
	Sci_Position max = pos + length;

	styler.StartAt(pos);
	styler.StartSegment(pos);

   StyleContext sc(startPos, max - startPos, initStyle, styler);

   // store information for neuron and definition highlighting
   kwType kwLast = kwOther;

	if (initStyle == SCE_NESTML_COMMENTBLOCK) {
		ResumeBlockComment(styler, pos, max, styler.GetLineState(styler.GetLine(pos) - 1));
	} else if (initStyle == SCE_NESTML_COMMENTLINE) {
		ResumeLineComment(styler, pos, max);
	} else if (initStyle == SCE_NESTML_STRING) {
		ResumeString(styler, pos, max);
	}

	while (pos < max) {
		int c = styler.SafeGetCharAt(pos, '\0');
		int n = styler.SafeGetCharAt(pos + 1, '\0');
		int n2 = styler.SafeGetCharAt(pos + 2, '\0');

		if (IsWhitespace(c)) {
			ScanWhitespace(styler, pos, max);
		} else if (c == '#' || (c == '/' && n == '*')) {
			ScanComments(styler, pos, max);
		} else if (IsWordStart(c)) {
			ScanIdentifier(styler, sc, pos, keywords, kwLast);
		} else if (IsADigit(c)) {
			ScanNumber(styler, pos);
		} else if (IsThreeCharOperator(c, n, n2)) {
			pos += 3;
			styler.ColourTo(pos - 1, SCE_NESTML_OPERATOR);
		} else if (IsTwoCharOperator(c, n)) {
			pos += 2;
			styler.ColourTo(pos - 1, SCE_NESTML_OPERATOR);
		} else if (IsOneCharOperator(c)) {
			pos++;
			styler.ColourTo(pos - 1, SCE_NESTML_OPERATOR);
		} else if (c == '"') {
			pos++;
			ResumeString(styler, pos, max);
		} else {
			pos++;
			styler.ColourTo(pos - 1, SCE_NESTML_LEXERROR);
		}
	}
	styler.ColourTo(pos - 1, SCE_NESTML_DEFAULT);
	styler.Flush();
}

void SCI_METHOD LexerNESTML::Fold(Sci_PositionU startPos, Sci_Position length, int initStyle, IDocument *pAccess) {

	if (!options.fold)
		return;

	LexAccessor styler(pAccess);

	Sci_PositionU endPos = startPos + length;
	int visibleChars = 0;
	bool inLineComment = false;
	Sci_Position lineCurrent = styler.GetLine(startPos);
	int levelCurrent = SC_FOLDLEVELBASE;
	if (lineCurrent > 0)
		levelCurrent = styler.LevelAt(lineCurrent-1) >> 16;
	Sci_PositionU lineStartNext = styler.LineStart(lineCurrent+1);
	int levelMinCurrent = levelCurrent;
	int levelNext = levelCurrent;
	char chNext = styler[startPos];
	int styleNext = styler.StyleAt(startPos);
	int style = initStyle;
	const bool userDefinedFoldMarkers = !options.foldExplicitStart.empty() && !options.foldExplicitEnd.empty();
	for (Sci_PositionU i = startPos; i < endPos; i++) {
		char ch = chNext;
		chNext = styler.SafeGetCharAt(i + 1);
		int stylePrev = style;
		style = styleNext;
		styleNext = styler.StyleAt(i + 1);
		bool atEOL = i == (lineStartNext-1);
		if (style == SCE_NESTML_COMMENTLINE)
			inLineComment = true;
		if (options.foldComment && options.foldCommentMultiline && IsStreamCommentStyle(style) && !inLineComment) {
			if (!IsStreamCommentStyle(stylePrev)) {
				levelNext++;
			} else if (!IsStreamCommentStyle(styleNext) && !atEOL) {
				// Comments don't end at end of line and the next character may be unstyled.
				levelNext--;
			}
		}
		if (options.foldComment && options.foldCommentExplicit && ((style == SCE_NESTML_COMMENTLINE) || options.foldExplicitAnywhere)) {
			if (userDefinedFoldMarkers) {
				if (styler.Match(i, options.foldExplicitStart.c_str())) {
					levelNext++;
				} else if (styler.Match(i, options.foldExplicitEnd.c_str())) {
					levelNext--;
				}
			} else {
				if ((ch == '/') && (chNext == '/')) {
					char chNext2 = styler.SafeGetCharAt(i + 2);
					if (chNext2 == '{') {
						levelNext++;
					} else if (chNext2 == '}') {
						levelNext--;
					}
				}
			}
		}
		if (options.foldSyntaxBased && (style == SCE_NESTML_OPERATOR)) {
			if (ch == '{') {
				// Measure the minimum before a '{' to allow
				// folding on "} else {"
				if (levelMinCurrent > levelNext) {
					levelMinCurrent = levelNext;
				}
				levelNext++;
			} else if (ch == '}') {
				levelNext--;
			}
		}
		if (!IsASpace(ch))
			visibleChars++;
		if (atEOL || (i == endPos-1)) {
			int levelUse = levelCurrent;
			if (options.foldSyntaxBased && options.foldAtElse) {
				levelUse = levelMinCurrent;
			}
			int lev = levelUse | levelNext << 16;
			if (visibleChars == 0 && options.foldCompact)
				lev |= SC_FOLDLEVELWHITEFLAG;
			if (levelUse < levelNext)
				lev |= SC_FOLDLEVELHEADERFLAG;
			if (lev != styler.LevelAt(lineCurrent)) {
				styler.SetLevel(lineCurrent, lev);
			}
			lineCurrent++;
			lineStartNext = styler.LineStart(lineCurrent+1);
			levelCurrent = levelNext;
			levelMinCurrent = levelCurrent;
			if (atEOL && (i == static_cast<Sci_PositionU>(styler.Length()-1))) {
				// There is an empty line at end of file so give it same level and empty
				styler.SetLevel(lineCurrent, (levelCurrent | levelCurrent << 16) | SC_FOLDLEVELWHITEFLAG);
			}
			visibleChars = 0;
			inLineComment = false;
		}
	}
}

LexerModule lmNESTML(SCLEX_NESTML, LexerNESTML::LexerFactoryNESTML, "NESTML", NESTMLWordLists);
