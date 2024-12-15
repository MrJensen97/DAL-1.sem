CREATE TABLE Biler.Forhandler (
    ForhandlerId INT PRIMARY KEY, -- Unikt ID for forhandleren
    #Forhandlernavn VARCHAR(50) NOT NULL, -- Navn på forhandleren
    Location VARCHAR(100) -- Adresse på forhandleren
);

-- Opret tabel "Biler"
CREATE TABLE Biler.Biler (
    CarId INT PRIMARY KEY, -- Unikt ID for bilen
    Model VARCHAR(100) NOT NULL, -- Bilens model
    FraLand VARCHAR(2), -- Beskrivelse af land
    Link VARCHAR (100), -- Link til bilens side
    ForhandlerId INT, -- Fremmednøgle der linker til CVRnummer i "Forhandler"
    FOREIGN KEY (ForhandlerId) REFERENCES Forhandler(ForhandlerId)
        ON DELETE CASCADE ON UPDATE CASCADE
);

-- Opret tabel "Bilpriser"
CREATE TABLE Biler.Bilpriser (
    CarId INT, -- Fremmednøgle der linker til CarId i "Biler"
    Dato DATE NOT NULL, -- Alder på bilen
    Pris INT NOT NULL, -- Pris på bilen
    Km INT NOT NULL, -- Kilometer på bilen
    Scrapedate DATETIME NOT NULL, -- Scrapedate på indhentning af data
    FOREIGN KEY (CarId) REFERENCES Biler(CarId)
        ON DELETE CASCADE ON UPDATE CASCADE
);