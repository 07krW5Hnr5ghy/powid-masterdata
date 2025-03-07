package com.proyect.masterdata.dto;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class GeneralStockDTO {
    private UUID id;
    private String user;
    public String productSku;
    public String model;
    public String color;
    public String size;
    public Integer quantity;
    public OffsetDateTime registrationDate;
    public OffsetDateTime updateDate;
}
