package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.AuditDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IAudit;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("audit")
@AllArgsConstructor
public class AuditController {
    private final IAudit iAudit;
    @GetMapping()
    public ResponseEntity<Page<AuditDTO>> list(
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "event",required = false) String event,
            @RequestParam(value = "ruc",required = false) String ruc,
            @RequestParam(value = "registrationStartDate",required = false) Date registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) Date registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) Date updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) Date updateEndDate,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<AuditDTO>> result = iAudit.list(user,event,ruc,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
