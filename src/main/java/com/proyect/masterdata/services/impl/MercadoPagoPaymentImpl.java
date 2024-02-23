package com.proyect.masterdata.services.impl;

import com.mercadopago.MercadoPagoConfig;
import com.mercadopago.client.payment.PaymentClient;
import com.mercadopago.client.preference.PreferenceBackUrlsRequest;
import com.mercadopago.client.preference.PreferenceClient;
import com.mercadopago.client.preference.PreferenceItemRequest;
import com.mercadopago.client.preference.PreferenceRequest;
import com.mercadopago.exceptions.MPApiException;
import com.mercadopago.exceptions.MPException;
import com.mercadopago.resources.payment.Payment;
import com.mercadopago.resources.preference.Preference;
import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IMercadoPagoPayment;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;

@Service
@RequiredArgsConstructor
@Log4j2
public class MercadoPagoPaymentImpl implements IMercadoPagoPayment {
    @Override
    public String sendPayment(Double netAmount, Subscription subscription, User user) throws InternalErrorExceptions, BadRequestExceptions {
        try{
            PreferenceItemRequest itemRequest = PreferenceItemRequest.builder()
                    .title("Pago subscripcion " + subscription.getName() + " powip.")
                    .quantity(1)
                    .unitPrice(new BigDecimal(netAmount).setScale(2, RoundingMode.HALF_EVEN))
                    .currencyId("USD")
                    .build();
            List<PreferenceItemRequest> items = new ArrayList<>();
            items.add(itemRequest);

            PreferenceBackUrlsRequest backUrls = PreferenceBackUrlsRequest.builder()
                    .failure("https://google.com")
                    .pending("https://youtube.com")
                    .success("https://youtube.com")
                    .build();

            PreferenceRequest preferenceRequest = PreferenceRequest.builder()
                    .items(items)
                    .backUrls(backUrls)
                    .binaryMode(true)
                    .notificationUrl("https://4b8d-2800-484-d57f-3830-c4ed-ebe9-a1c4-d3e3.ngrok-free.app/masterdata/webhook")
                    .build();

            PreferenceClient preferenceClient = new PreferenceClient();

            Preference preference = preferenceClient.create(preferenceRequest);
            System.out.println(preference.getResponse());
            return preference.getInitPoint();

        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        } catch (MPException | MPApiException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String checkPaymentStatus(Long paymentId, String type) throws InternalErrorExceptions, BadRequestExceptions, MPException, MPApiException {
        if(paymentId != null & Objects.equals(type, "payment")){
            PaymentClient paymentClient = new PaymentClient();
            Payment newPayment = paymentClient.get(paymentId);
            System.out.println(newPayment.getId());
            return "Payment Successful";
        }else { return "Payment failed"; }
    }
}
