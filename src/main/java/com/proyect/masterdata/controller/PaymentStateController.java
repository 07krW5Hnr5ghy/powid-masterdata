package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentState;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IPaymentState;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
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

    @Operation(summary = "lista los estados de pago",
            description = "Lista los estados de pago")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Success",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))}),
            @ApiResponse(responseCode = "400", description = "Bad Request",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "401", description = "Unauthorized",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "403", description = "ForbiddenForbidden",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "404", description = "Not Found",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "409", description = "Conflict",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "500", description = "Internal Server Error",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))})
    })

    @GetMapping()
    public ResponseEntity<List<PaymentStateDTO>> listPaymentState() throws BadRequestExceptions{
        List<PaymentStateDTO> result = iPaymentState.listPaymentState();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Registrar estados de pago",
            description = "Registrar estados de pago")
    @PostMapping()
    public ResponseEntity<ResponsePaymentState> addPaymentState(@RequestParam("name") String name) throws BadRequestExceptions {
        ResponsePaymentState result = iPaymentState.addPaymentState(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Eliminar estados de pago",
            description = "Eliminar estados de pago")
    @DeleteMapping()
    public ResponseEntity<ResponsePaymentState> deletePaymentState(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponsePaymentState result = iPaymentState.deletePaymentState(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar estados de pago",
            description = "Editar estados de pago")
    @PutMapping()
    public ResponseEntity<PaymentStateDTO> updatePaymentState(@RequestBody PaymentStateDTO data) throws BadRequestExceptions{
        PaymentStateDTO result = iPaymentState.updatePaymentState(data.getName(), data.getId());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
