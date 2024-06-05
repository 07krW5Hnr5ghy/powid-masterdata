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
import com.mercadopago.resources.payment.PaymentFeeDetail;
import com.mercadopago.resources.preference.Preference;
import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.MembershipPaymentRepository;
import com.proyect.masterdata.repository.MembershipRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IMembershipPayment;
import com.proyect.masterdata.services.IMercadoPagoPayment;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.codec.digest.HmacUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.MessageFormat;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
@Log4j2
public class MercadoPagoPaymentImpl implements IMercadoPagoPayment {
    @Value("${mercadopago.api.token}")
    private String mercadoPagoToken;
    private final IMembershipPayment iMembershipPayment;
    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final MembershipPaymentRepository membershipPaymentRepository;
    private final IAudit iAudit;
    @Value("${mercadopago.notification.url}")
    private String mercadoPagoNotificationUrl;
    @Value("${mercadopago.secret.key}")
    private String mercadoPagoSecretKey;
    @Override
    public CompletableFuture<String> sendPayment(Double netAmount, Subscription subscription, List<String> modules, User user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            MercadoPagoConfig.setAccessToken(mercadoPagoToken);
            MercadoPagoConfig.setLoggingLevel(Level.FINEST);

            try{
                PreferenceItemRequest itemRequest = PreferenceItemRequest.builder()
                        .title("Pago subscripcion " + subscription.getName() + " powip.")
                        .quantity(1)
                        .unitPrice(new BigDecimal(netAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .currencyId("PEN")
                        .build();
                List<PreferenceItemRequest> items = new ArrayList<>();
                items.add(itemRequest);

                PreferenceBackUrlsRequest backUrls = PreferenceBackUrlsRequest.builder()
                        .failure("https://google.com")
                        .pending("https://youtube.com")
                        .success("https://youtube.com")
                        .build();

                Map<String, Object> metadata = new HashMap<>();

                metadata.put("userId",user.getUsername());
                metadata.put("subscriptionName",subscription.getName());
                metadata.put("modules",modules);

                PreferenceRequest preferenceRequest = PreferenceRequest.builder()
                        .items(items)
                        .metadata(metadata)
                        .backUrls(backUrls)
                        .binaryMode(true)
                        .notificationUrl(mercadoPagoNotificationUrl)
                        .build();

                PreferenceClient preferenceClient = new PreferenceClient();

                Preference preference = preferenceClient.create(preferenceRequest);
                iAudit.save("SEND_MERCADO_PAGO_PAYMENT","SEND MERCADO PAGO PAYMENT "+preference.getId()+".",user.getUsername());
                return preference.getInitPoint();

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (MPException | MPApiException e) {
                log.error(e.getMessage());
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> registerPayment(Long paymentId, String type, String requestIdHeader, String signatureHeader) throws InternalErrorExceptions, BadRequestExceptions, MPException, MPApiException {
        return CompletableFuture.supplyAsync(()->{
            MembershipPayment membershipPayment;
            try{
                if(paymentId != null & Objects.equals(type, "payment")){
                    PaymentClient paymentClient = new PaymentClient();
                    Payment newPayment = paymentClient.get(paymentId);
                    User user = userRepository.findById(Long.parseLong(newPayment.getMetadata().get("user_id").toString())).orElse(null);
                    if(user==null){
                        throw new BadRequestExceptions(Constants.ErrorUser);
                    }
                    System.out.println(newPayment.getStatus());

                    if(!Objects.equals(newPayment.getStatus(), "approved")){
                        throw new BadRequestExceptions(Constants.ErrorMercadoPagoPaymentFailed);
                    }

                    System.out.println("mercado pago secret key");
                    System.out.println(mercadoPagoSecretKey);
                    String exampleSignatureHeader = "ts=1704908010,v1=618c85345248dd820d5fd456117c2ab2ef8eda45a0282ff693eac24131a5e839";
                    Map<String,String> signatureMap = new HashMap<>();
                    Pattern pattern = Pattern.compile("(\\w+)=([^,]+)");
                    Matcher matcher = pattern.matcher(signatureHeader);
                    while (matcher.find()) {
                        String key = matcher.group(1);
                        String value = matcher.group(2);
                        signatureMap.put(key, value);
                    }
                    String template = "id:[{0}];request-id:[{1}];ts:[{2}]";
                    String formattedString = MessageFormat.format(template, paymentId, requestIdHeader, signatureMap.get("ts"));
                    System.out.println(formattedString);
                    String cyphedSignature = new HmacUtils("HmacSHA256", mercadoPagoSecretKey).hmacHex(formattedString);
                    System.out.println("cyphed signature");
                    System.out.println(cyphedSignature);
                    if(!Objects.equals(mercadoPagoSecretKey, cyphedSignature)){
                        throw new BadRequestExceptions(Constants.ErrorMercadoPagoOrigin);
                    }
                    double fee = 0.00;
                    for(PaymentFeeDetail paymentFeeDetail : newPayment.getFeeDetails()){
                        fee += paymentFeeDetail.getAmount().doubleValue();
                    }

                    System.out.println(newPayment.getMetadata());
                    List<String> moduleNames = (List<String>) newPayment.getMetadata().get("modules");
                    RequestMembershipPayment requestMembershipPayment = RequestMembershipPayment.builder()
                            .netAmount(newPayment.getTransactionDetails().getNetReceivedAmount().doubleValue())
                            .taxAmount(newPayment.getTaxesAmount().doubleValue())
                            .paymentGatewayFee(fee)
                            .grossAmount(newPayment.getTransactionDetails().getNetReceivedAmount().doubleValue() + newPayment.getTaxesAmount().doubleValue() + fee)
                            .subscriptionName(newPayment.getMetadata().get("subscription_name").toString())
                            .demo(false)
                            .modules(moduleNames)
                            .paymentGateway("mercado pago")
                            .build();
                    iMembershipPayment.save(requestMembershipPayment,newPayment.getMetadata().get("user_id").toString());
                    iAudit.save("ADD_MERCADO_PAGO_PAYMENT","ADD MERCADO PAGO PAYMENT "+paymentId+".",user.getUsername());
                }else{
                    throw new BadRequestExceptions(Constants.ErrorMercadoPagoPayment);
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | MPException | MPApiException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
