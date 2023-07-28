package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentMethod;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPaymentMethod;
import io.swagger.v3.oas.annotations.Operation;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment-method")
@AllArgsConstructor
public class PaymentMethodController {

    private final IPaymentMethod iPaymentMethod;

    @Operation(summary = "lista los metodos de pago ",
            description = "Lista los metodos de pago maestro")
    @GetMapping()
    public ResponseEntity<List<PaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions {
        List<PaymentMethodDTO>  result = iPaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping()
    public ResponseEntity<ResponsePaymentMethod> addPaymentMethod(@RequestParam("name") String name) throws BadRequestExceptions{
        ResponsePaymentMethod result = iPaymentMethod.addPaymentMethod(name);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<String> deletePaymentMethod(@RequestBody Long id) throws BadRequestExceptions{
        iPaymentMethod.deletePaymentMethod(id);
        return new ResponseEntity<>("Payment method with id : " + id + " deleted.",HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<String> updatePaymentMethod(@RequestBody PaymentMethodDTO data) throws BadRequestExceptions{
        iPaymentMethod.updatePaymentMethod(data.getName(), data.getId());
        return new ResponseEntity<>("Payment method with id : " + data.getId() + "change name to " + data.getName() + ".",HttpStatus.OK);
    }
}
