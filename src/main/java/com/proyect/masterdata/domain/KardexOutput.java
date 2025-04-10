package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableKardexOuput, schema = Constants.schemaStock)
public class KardexOutput {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "kardex_output_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "kardex_input_id")
    private UUID kardexInputId;

    @Column(name = "lot_number")
    private Long lotNumber;

    @Column(name = "kardex_operation_type_id")
    private UUID kardexOperationTypeId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name="kardex_input_id",columnDefinition = "kardexInputId",insertable = false,updatable = false)
    private KardexInput kardexInput;

    @ManyToOne()
    @JoinColumn(name="kardex_operation_type_id",columnDefinition = "kardexOperationTypeId",insertable = false,updatable = false)
    private KardexOperationType kardexOperationType;
}
