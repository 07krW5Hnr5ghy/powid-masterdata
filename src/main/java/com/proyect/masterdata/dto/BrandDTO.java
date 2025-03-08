package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BrandDTO {
    private UUID id;
    private String name;
    private String sku;
    private String client;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String tokenUser;
    private Boolean status;
}
