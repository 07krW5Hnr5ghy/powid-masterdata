package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestUserType {
    private UUID code;
    private String usertype;
    private final boolean status = true;
    private String user;
}
