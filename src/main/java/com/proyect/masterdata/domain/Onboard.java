package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOnboarding, schema = Constants.schemaManagement)
public class Onboard {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "onboard_id")
    private String id;

    @Column(name = "ecommerce", nullable = false)
    private Boolean ecommerce;

    @Column(name = "users_minimum", nullable = false)
    private Integer usersMinimum;

    @Column(name = "users_maximum", nullable = false)
    private Integer usersMaximum;

    @Column(name = "billing", nullable = false)
    private Boolean billing;

    @Column(name = "comment", nullable = false)
    private String comment;

    @Column(name = "demo", nullable = false)
    private Boolean demo;

    @Column(name = "client_id", nullable = false)
    private Long clientId;

    @Column(name = "entry_channel_id", nullable = false)
    private Long entryChannelId;

    @Column(name = "category_id", nullable = false)
    private Long categoryId;

    @OneToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "entry_channel_id", columnDefinition = "entryChannelId", insertable = false, updatable = false)
    private EntryChannel entryChannel;

    @ManyToOne
    @JoinColumn(name = "category_id", columnDefinition = "categoryId", insertable = false, updatable = false)
    private Category category;

}
