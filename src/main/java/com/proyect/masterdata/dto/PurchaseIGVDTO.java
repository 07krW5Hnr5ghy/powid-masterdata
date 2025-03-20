package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PurchaseIGVDTO {
    private UUID id;
    private String name;
    private Double value;
    private String user;
    private Boolean percentage;
    private Boolean status;
}
