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
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("mercado-pago")
@RequiredArgsConstructor
public class MercadoPagoController {

    @Value("${mercadopago.api.token}")
    private String mercadoPagoToken;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
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
                    .notificationUrl("https://2861-2800-484-d57f-3830-e900-8538-4734-51c6.ngrok-free.app/masterdata/webhook")
                    .build();

            PreferenceClient preferenceClient = new PreferenceClient();

            Preference preference = preferenceClient.create(preferenceRequest);
            System.out.println(preference.getResponse());
            return preference.getId();

        }catch (MPException | MPApiException e){
            return e.toString();
        }
    }
}
