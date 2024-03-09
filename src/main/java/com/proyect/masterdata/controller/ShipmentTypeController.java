package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IShipmentType;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("shipment-type")
@AllArgsConstructor
public class ShipmentTypeController {

    private final IShipmentType iShipmentType;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SHIPMENT_TYPE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseSuccess result = iShipmentType.save(name,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:SHIPMENT_TYPE_GET')")
    public ResponseEntity<List<String>> list() throws BadRequestExceptions {
        List<String> result = iShipmentType.list();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
