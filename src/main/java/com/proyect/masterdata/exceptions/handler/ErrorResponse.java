package com.proyect.masterdata.exceptions.handler;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
@AllArgsConstructor
public class ErrorResponse {
    private int code;
    private String errorCode;
    private String message;
}
