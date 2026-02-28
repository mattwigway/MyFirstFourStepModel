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
    pub fn new(filename: String) -> Result<Self> {
        let file = File::create(filename)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        let writer = ZipWriter::new(file);
        Ok(Self { writer: Some(writer) })
    }

    pub fn write_entry(&mut self, name: String, body: String) -> Result<()> {
        match self.writer.as_mut() {
            Some(writer) => {
                let opts = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
                writer.start_file(name, opts)
                .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;

                writer.write_all(body.as_bytes())
                .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;

                Ok(())
            },
            None => Err(Error::Other("File closed".to_string()))
        }
    }

    pub fn write_file(&mut self, name: String, file: String) -> Result<()> {
        match self.writer.as_mut() {
            Some(writer) => {
                let mut fh = File::open(file)
                    .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;

                let opts = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
                writer.start_file(name, opts)
                    .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
                std::io::copy(&mut fh, writer)
                    .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
                Ok(())
            },
            None => Err(Error::Other("File closed".to_string()))
        }
    }

    // self rather than &self to take ownership and prevent future use
    pub fn finish(&mut self) -> Result<()> {
        match self.writer {
            Some(_) => {
                self.writer.take().unwrap().finish()
                    .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
                Ok(())
            },
            None => Err(Error::Other("File closed".to_string()))
        }
    }
}

#[extendr]
pub struct ArchiveReader {
    reader: ZipArchive<Cursor<Vec<u8>>>
}



#[extendr]
impl ArchiveReader {
    pub fn new(path: String) -> extendr_api::Result<ArchiveReader> {
        let mut buf = Vec::new();

        if path.starts_with("https://") || path.starts_with("http://") {
            ureq::get(path)
                .header("User-Agent", "MyFirstFourStepModel")
                .call()
                .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?
                .body_mut()
                .as_reader()
                .read_to_end(&mut buf)
                .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        } else {
            // treat as local file
            File::open(path)
                .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?
                .read_to_end(&mut buf)
                .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        }

        let cursor = std::io::Cursor::new(buf);
        let reader = ZipArchive::new(cursor)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;

        Ok(ArchiveReader { reader: reader })
    }

    pub fn entries(&self) -> Result<Strings> {
        Ok(Strings::from_iter(self.reader.file_names()))
    }

    pub fn get_entry_as_string(&mut self, name: String) -> Result<String> {
        let mut file = self.reader.by_name(&name)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;

        let mut buf = Vec::new();
        file.read_to_end(&mut buf)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        let result = String::from_utf8(buf)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        Ok(result)
    }

    pub fn extract_entry(&mut self, name: String, target: String) -> Result<()> {
        let mut inf = self.reader.by_name(&name)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        let mut outf = File::create(target)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        std::io::copy(&mut inf, &mut outf)
            .or_else(|err| Err(extendr_api::error::Error::Other(err.to_string())))?;
        Ok(())
    }
}

extendr_module! {
    mod serialization;

    impl ArchiveReader;
    impl ArchiveWriter;
}