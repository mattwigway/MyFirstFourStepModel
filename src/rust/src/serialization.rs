use std::{fs::File, io::{Cursor, Read}};

use extendr_api::prelude::*;

use zip::{ZipArchive, ZipWriter, write::SimpleFileOptions};
use std::io::Write;

#[extendr]
pub struct ArchiveWriter {
    // This is an option so we can dispose of it safely after finish() is called
    writer: Option<ZipWriter<File>>
}

#[extendr]
impl ArchiveWriter {
    pub fn new(filename: String) -> std::result::Result<Self, Box<dyn std::error::Error>> {
        let file = File::create(filename)?;
        let writer = ZipWriter::new(file);
        Ok(Self { writer: Some(writer) })
    }

    pub fn write_entry(&mut self, name: String, body: String) -> std::result::Result<(), Box<dyn std::error::Error>> {
        match self.writer.as_mut() {
            Some(writer) => {
                let opts = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
                writer.start_file(name, opts)?;
                writer.write_all(body.as_bytes())?;
                Ok(())
            },
            None => Err(Box::new(std::io::Error::new(std::io::ErrorKind::NotConnected, "File closed")))
        }
    }

    pub fn write_file(&mut self, name: String, file: String) -> std::result::Result<(), Box<dyn std::error::Error>> {
        match self.writer.as_mut() {
            Some(writer) => {
                let mut fh = File::open(file)?;
                let opts = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
                writer.start_file(name, opts)?;
                std::io::copy(&mut fh, writer)?;
                Ok(())
            },
            None => Err(Box::new(std::io::Error::new(std::io::ErrorKind::NotConnected, "File closed")))
        }
    }

    // self rather than &self to take ownership and prevent future use
    pub fn finish(&mut self) -> std::result::Result<(), Box<dyn std::error::Error>> {
        match self.writer {
            Some(_) => {
                self.writer.take().unwrap().finish()?;
                Ok(())
            },
            None => Err(Box::new(std::io::Error::new(std::io::ErrorKind::NotConnected, "File closed")))
        }
    }
}

#[extendr]
pub struct ArchiveReader {
    reader: ZipArchive<Cursor<Vec<u8>>>
}

#[extendr]
impl ArchiveReader {
    pub fn new(path: String) -> std::result::Result<ArchiveReader, Box<dyn std::error::Error>> {
        let mut buf = Vec::new();

        if path.starts_with("https://") || path.starts_with("http://") {
            ureq::get(path)
                .header("User-Agent", "MyFirstFourStepModel")
                .call()?
                .body_mut()
                .as_reader()
                .read_to_end(&mut buf)?;
        } else {
            // treat as local file
            File::open(path)?
                .read_to_end(&mut buf)?;
        }

        let cursor = std::io::Cursor::new(buf);
        let reader = ZipArchive::new(cursor)?;

        Ok(ArchiveReader { reader: reader })
    }

    pub fn entries(&self) -> Result<Strings> {
        Ok(Strings::from_iter(self.reader.file_names()))
    }

    pub fn get_entry_as_string(&mut self, name: String) -> std::result::Result<String, Box<dyn std::error::Error>> {
        let mut file = self.reader.by_name(&name)?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)?;
        let result = String::from_utf8(buf)?;
        Ok(result)
    }

    pub fn extract_entry(&mut self, name: String, target: String) -> std::result::Result<(), Box<dyn std::error::Error>> {
        let mut inf = self.reader.by_name(&name)?;
        let mut outf = File::create(target)?;
        std::io::copy(&mut inf, &mut outf)?;
        Ok(())
    }
}

extendr_module! {
    mod serialization;

    impl ArchiveReader;
    impl ArchiveWriter;
}