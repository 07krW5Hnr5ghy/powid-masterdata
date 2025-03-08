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
public class CategoryProductDTO {
    private UUID id;
    private String user;
    private String name;
    private String sku;
    private String sizeType;
    private String unitType;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private Boolean status;
}
