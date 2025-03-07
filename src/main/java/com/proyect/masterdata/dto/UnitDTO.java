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
public class UnitDTO {
    private String name;
    private String unitType;
    private UUID id;
    private String user;
    private Boolean status;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
