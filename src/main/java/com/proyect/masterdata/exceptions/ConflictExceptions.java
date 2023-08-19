package com.proyect.masterdata.exceptions;

import lombok.Data;

@Data
public class ConflictExceptions extends RuntimeException{
    public ConflictExceptions(String message) {
        super(message);
    }


}
