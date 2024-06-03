package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IAuditEvent;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("audit-event")
@AllArgsConstructor
public class AuditEventController {
    private final IAuditEvent iAuditEvent;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iAuditEvent.saveAsync(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iAuditEvent.delete(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping
    public ResponseEntity<List<String>> list() throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<String>> result = iAuditEvent.list();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
