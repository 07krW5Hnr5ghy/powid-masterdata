package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICancellationReason;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("cancellation-reason")
@AllArgsConstructor
public class CancellationReasonController {

    private final ICancellationReason iCancellationReason;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CANCELLATION_REASON_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions{
        ResponseSuccess result = iCancellationReason.save(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLES:CUSTOMER_SERVICE') and hasAuthority('ACCESS:CANCELLATION_REASON_GET')")
    public ResponseEntity<List<String>> list() throws BadRequestExceptions{
        List<String> result = iCancellationReason.list();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
