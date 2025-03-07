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
public class SaleChannelDTO {
    private UUID id;
    private Boolean status;
    private String name;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private String user;
}
