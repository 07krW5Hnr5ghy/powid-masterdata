package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.sql.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestModule {
    private Long code;
    private String name;
    private double price;
    private int statusModule;
    private boolean status;
    private String user;
}
