package com.proyect.masterdata.controller;

import com.mercadopago.MercadoPagoConfig;
import com.mercadopago.client.payment.PaymentClient;
import com.mercadopago.client.payment.PaymentCreateRequest;
import com.mercadopago.client.payment.PaymentPayerRequest;
import com.mercadopago.client.preference.PreferenceBackUrlsRequest;
import com.mercadopago.client.preference.PreferenceClient;
import com.mercadopago.client.preference.PreferenceItemRequest;
import com.mercadopago.client.preference.PreferenceRequest;
import com.mercadopago.exceptions.MPApiException;
import com.mercadopago.exceptions.MPException;
import com.mercadopago.resources.payment.Payment;
import com.mercadopago.resources.preference.Preference;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.services.IMercadoPagoPayment;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("mercado-pago")
@RequiredArgsConstructor
public class MercadoPagoController {

    @Value("${mercadopago.api.token}")
    private String mercadoPagoToken;

    private final IMercadoPagoPayment iMercadoPagoPayment;

    @PostMapping()
    public String mercadoPagoTest(){
        MercadoPagoConfig.setAccessToken(mercadoPagoToken);
        MercadoPagoConfig.setLoggingLevel(Level.FINEST);


        try {
            PreferenceItemRequest itemRequest = PreferenceItemRequest.builder()
                    .title("first payment test")
                    .quantity(1)
                    .unitPrice(new BigDecimal(100))
                    .currencyId("PEN")
                    .build();

            List<PreferenceItemRequest> items = new ArrayList<>();
            items.add(itemRequest);

            PreferenceBackUrlsRequest backUrls = PreferenceBackUrlsRequest.builder()
                    .failure("https://youtube.com")
                    .pending("https://youtube.com")
                    .success("https://youtube.com")
                    .build();

            PreferenceRequest preferenceRequest = PreferenceRequest.builder()
                    .items(items)
                    .backUrls(backUrls)
                    .notificationUrl("https://4b8d-2800-484-d57f-3830-c4ed-ebe9-a1c4-d3e3.ngrok-free.app/masterdata/webhook")
                    .build();

            PreferenceClient preferenceClient = new PreferenceClient();

            Preference preference = preferenceClient.create(preferenceRequest);
            System.out.println(preference.getResponse());
            return preference.getNotificationUrl();

        }catch (MPException | MPApiException e){
            return e.toString();
        }
    }

    @PostMapping(value = "check-status")
    public ResponseEntity<ResponseSuccess> checkPaymentStatus(
            @RequestParam(value = "id",required = false) String id,
            @RequestParam(value = "topic",required = false) String topic,
            @RequestParam(value = "data.id",required = false) Long paymentId,
            @RequestParam(value = "type",required = false) String type,
            @RequestHeader HttpHeaders headers
            ) throws MPException, MPApiException, ExecutionException, InterruptedException {
        String requestIdHeader = headers.getFirst("X-Request-Id");
        String signatureHeader = headers.getFirst("X-Signature");
        CompletableFuture<ResponseSuccess> result = iMercadoPagoPayment.registerPayment(paymentId,type,requestIdHeader,signatureHeader);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
