package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentState;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-state")
@AllArgsConstructor
public class PaymentStateController {

    private final IPaymentState iPaymentState;

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentState.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/payment-states")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iPaymentState.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<PaymentStateDTO> update(
            @RequestBody() RequestPaymentState requestPaymentState
    ) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.update(requestPaymentState);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iPaymentState.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("/list")
    public ResponseEntity<List<PaymentStateDTO>> listPaymentState() throws BadRequestExceptions {
        List<PaymentStateDTO> result = iPaymentState.listPaymentState();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<PaymentStateDTO>> listStatusFalse() throws BadRequestExceptions {
        List<PaymentStateDTO> result = iPaymentState.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<PaymentStateDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        PaymentStateDTO result = iPaymentState.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
