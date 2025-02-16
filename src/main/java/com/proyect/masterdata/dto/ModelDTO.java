package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ModelDTO {
    private String name;
    private String brand;
    private String sku;
    private String user;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
