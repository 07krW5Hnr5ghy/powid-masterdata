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
public class UnitTypeDTO {
    private String name;
    private UUID id;
    private String user;
    private Boolean status;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
