package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentState;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/payment-state")
@AllArgsConstructor
public class PaymentStateController {

    private final IPaymentState iPaymentState;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentState.save(name, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-states", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names, @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentState.saveAll(names, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PaymentStateDTO> update(
            @RequestBody() RequestPaymentState requestPaymentState) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.update(requestPaymentState);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iPaymentState.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list")
    public ResponseEntity<List<PaymentStateDTO>> listPaymentState() throws BadRequestExceptions {
        List<PaymentStateDTO> result = iPaymentState.listPaymentState();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<PaymentStateDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<PaymentStateDTO> result = iPaymentState.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/statusFalse")
    public ResponseEntity<Page<PaymentStateDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<PaymentStateDTO> result = iPaymentState.listStatusFalse(name, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<PaymentStateDTO> findByCode(
            @RequestParam("code") Long code) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
