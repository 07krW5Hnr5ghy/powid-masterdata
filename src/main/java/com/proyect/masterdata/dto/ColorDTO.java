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
public class ColorDTO {
    private String name;
    private String user;
    private UUID id;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private Boolean status;
}
