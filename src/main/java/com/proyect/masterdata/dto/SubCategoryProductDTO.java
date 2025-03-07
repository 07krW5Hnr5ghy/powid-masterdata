package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SubCategoryProductDTO {
    private UUID id;
    private String name;
    private String sku;
    private String sizeType;
    private String categoryProduct;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private Boolean status;
    private String user;
}
