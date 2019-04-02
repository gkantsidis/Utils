// XSD Schema Include Normalizer
// To compile:
// csc filename.cs
//
// How to use:
//
// Arguments: [-q] input.xsd [output.xsd]
//
// input.xsd       - file to normalize
// output.xsd      - file to output, default is console
// -q              - quiet
//
// Example:
//
// filename.exe schema.xsd
//
//
// Code from https://docs.microsoft.com/en-us/sql/relational-databases/xml/preprocess-a-schema-to-merge-included-schemas?view=sql-server-2017
// with extension to ignore errors about existing declarations.

using System;
using System.Collections;
using System.IO;
using System.Xml;
using System.Xml.Schema;

public class XsdSchemaNormalizer {
    private static void xmlSchemaSet_ValidationEventHandler (object sender, ValidationEventArgs e) {
        if (e.Exception.Message.Contains ("has already been declared")) {
            // Ignore those errors
        } else {
            throw e.Exception;
        }
    }

    private static bool NormalizeXmlSchema (String url, TextWriter writer) {
        try {
            var ignoreError = new ValidationEventHandler (xmlSchemaSet_ValidationEventHandler);

            XmlTextReader txtRead = new XmlTextReader (url);
            XmlSchema sch = XmlSchema.Read (txtRead, null);

            // Compiling Schema
            sch.Compile (ignoreError);

            XmlSchema outSch =
                XmlSchemaIncludeNormalizer.BuildIncludeFreeXmlSchema (sch);

            outSch.Write (writer);
        } catch (Exception e) {
            Console.WriteLine (e.ToString ());
            return false;
        }
        return true;
    }

    public static void usage () {
        Console.WriteLine ("Arguments: [-q] [-v] input.xsd [output.xsd]\n");
        Console.WriteLine ("input.xsd       - file to normalize");
        Console.WriteLine ("output.xsd      - file to output, default is console");
        Console.WriteLine ("-q              - quiet");
    }

    public static void Main (String[] args) {
        if (args.GetLength (0) < 1) {
            usage ();
            return;
        }
        int argi = 0;
        bool quiet = false;
        if (args[argi] == "-q") {
            quiet = true;
            argi++;
        }

        if (argi == args.GetLength (0)) {
            usage ();
            return;
        }

        String url = args[argi];

        if (!quiet)
            Console.WriteLine ("Loading Schema: " + url);

        if (argi < (args.GetLength (0) - 1)) {
            if (!quiet)
                Console.WriteLine ("Outputing to file: " + args[argi + 1]);

            StreamWriter output =
                new StreamWriter (new FileStream (args[argi + 1], FileMode.Create));

            NormalizeXmlSchema (url, output);
        } else {
            NormalizeXmlSchema (url, Console.Out);
        }

    }
}

// A class to remove all <include> from a Xml Schema
//
public class XmlSchemaIncludeNormalizer {
    // Takes as input a XmlSchema which has includes in it
    // and the schema location uri of that XmlSchema
    //
    // Returns a "preprocessed" form of XmlSchema without any
    // includes. It still retains imports though. Also, it does
    // not propagate unhandled attributes
    //
    // It can throw any exception
    public static XmlSchema BuildIncludeFreeXmlSchema (XmlSchema inSch) {
        XmlSchema outSch = new XmlSchema ();

        AddSchema (outSch, inSch);

        return outSch;
    }

    // Adds everything in the second schema minus includes to
    // the first schema
    //
    private static void AddSchema (XmlSchema outSch, XmlSchema add) {
        outSch.AttributeFormDefault = add.AttributeFormDefault;
        outSch.BlockDefault = add.BlockDefault;
        outSch.ElementFormDefault = add.ElementFormDefault;
        outSch.FinalDefault = add.FinalDefault;
        outSch.Id = add.Id;
        outSch.TargetNamespace = add.TargetNamespace;
        outSch.Version = add.Version;

        AddTableToSchema (outSch, add.AttributeGroups);
        AddTableToSchema (outSch, add.Attributes);
        AddTableToSchema (outSch, add.Elements);
        AddTableToSchema (outSch, add.Groups);
        AddTableToSchema (outSch, add.Notations);
        AddTableToSchema (outSch, add.SchemaTypes);

        // Handle includes as a special case
        for (int i = 0; i < add.Includes.Count; i++) {
            if (!(add.Includes[i] is XmlSchemaInclude))
                outSch.Includes.Add (add.Includes[i]);
        }
    }

    // Adds all items in the XmlSchemaObjectTable to the specified XmlSchema
    //
    private static void AddTableToSchema (XmlSchema outSch,
        XmlSchemaObjectTable table) {
        IDictionaryEnumerator e = table.GetEnumerator ();

        while (e.MoveNext ()) {
            outSch.Items.Add ((XmlSchemaObject) e.Value);
        }
    }
}